extern crate zip;
extern crate reqwest;
extern crate chrono;
extern crate serde;
extern crate statrs;

pub mod wca_export;
pub mod wca_competitors;

use std::num::ParseIntError;
use std::io;
use std::collections::HashMap;
use std::convert::From;
use std::vec::Vec;
use std::ops::Deref;

use self::serde::{Serialize, Serializer};
use self::zip::result::ZipError;
use self::statrs::function::erf;
use self::chrono::Date;
use self::chrono::offset;


#[derive(Debug, Clone)]
pub struct DateW {
    date: Date<offset::Utc>
}

impl DateW {
    fn new(date: Date<offset::Utc>) -> DateW {
        DateW {
            date: date
        }
    }
}

impl Default for DateW {
    fn default() -> DateW {
        DateW::new(offset::Utc::today())
    }
}

impl Serialize for DateW {
    fn serialize<S>(&self, ser: S) -> Result<S::Ok, S::Error>
        where S: Serializer
    {
        ser.serialize_str(&format!("{}", **self))
    }
}

impl Deref for DateW {
    type Target = Date<offset::Utc>;

    fn deref(&self) -> &Self::Target {
        &self.date
    }
}

#[derive(Debug)]
pub enum WcaError {
    ZipE(ZipError),
    IoE(io::Error),
    NumE(ParseIntError),
    NetE(reqwest::Error),
    UrlE(reqwest::UrlError),
    ReadE(String),
    PersonE(String),
    CompE(String)
}

impl From<ZipError> for WcaError {
    fn from(e: ZipError) -> WcaError {
        WcaError::ZipE(e)
    }
}
impl From<io::Error> for WcaError {
    fn from(e: io::Error) -> WcaError {
        WcaError::IoE(e)
    }
}
impl From<ParseIntError> for WcaError {
    fn from(e: ParseIntError) -> WcaError {
        WcaError::NumE(e)
    }
}
impl From<reqwest::Error> for WcaError {
    fn from(e: reqwest::Error) -> WcaError {
        WcaError::NetE(e)
    }
}
impl From<reqwest::UrlError> for WcaError {
    fn from(e: reqwest::UrlError) -> WcaError {
        WcaError::UrlE(e)
    }
}


#[derive(Serialize, Debug, Default)]
pub struct WcaResults {
    pub people: HashMap<String, WcaPerson>, // Id: Person
    pub comps: HashMap<String, Competition>, // Id: Comp
    pub download_date: DateW,
}

#[derive(Serialize, Debug, Default)]
pub struct WcaPerson {
    pub name: String,
    pub times: HashMap<String, Vec<Time>> // Event: [times]
}

#[derive(Serialize, Debug)]
pub struct CompInfo<'a> {
    pub comp: &'a Competition,
    pub people: Vec<ExtendedPerson<'a>>,
    pub places_chances: HashMap<String, HashMap<String, Vec<f64>>> // Event: {id: [chances]...}
}

#[derive(Serialize, Debug)]
pub struct ExtendedPerson<'a> {
    pub person: &'a WcaPerson,
    pub id: String,
    pub current_avgs: HashMap<String, (Time, f64)>, // Event: (avg, dev)
}

impl WcaPerson {
    pub fn get_avgs(&self) -> HashMap<String, (Time, f64)> {
        let mut res = HashMap::new();
        for (event, times) in (*self).times.iter() {
            if let Some(( avg, dev )) = get_avg_stddev(times) {
                res.insert(event.clone(), ( Time::Time((avg * 100f64) as u16), dev ));
            }
        }
        res
    }

}

impl <'l> ExtendedPerson<'l> {
    // Algorithm taken from https://math.stackexchange.com/a/40236/244810
    pub fn chance_beating<'a>(&self, other: &'a ExtendedPerson, event: &'a str) -> f64 {
        match (self.current_avgs.get(event), other.current_avgs.get(event)) {
            (Some( &(ref m_avg_t, ref m_std) ), Some( &(ref o_avg_t, ref o_std) )) => {
                match ( m_avg_t.to_option_sec(), o_avg_t.to_option_sec() ) {
                    ( Some(m_avg), Some(o_avg) ) => {
                        let avg = m_avg - o_avg;
                        let std_2 = m_std * m_std + o_std * o_std;
                        let phi = (1f64 + erf::erf(-avg / f64::sqrt(std_2 * 2f64))) / 2f64;
                        phi
                    }
                    ( None, Some(_) ) => 0f64,
                    ( Some(_), None ) => 1f64,
                    _ => 0.5
                }
            }
            ( None, Some(_) ) => 0f64,
            ( Some(_), None ) => 1f64,
            _ => 0.5
        }
    }

    // Calculate the probability of placing in any place in a comp
    pub fn place_prob(&self, others: &[ExtendedPerson], event: &str) -> Vec<f64> {
        if others.len() == 0 {
            return vec![1f64];
        }
        let first = &others[0];
        let prob = self.chance_beating(first, &event);
        
        let prev = &others[1..];
        let prev_prob = self.place_prob(prev, event);

        let mut new_prob = vec![0f64; prev_prob.len() + 1];

        for i in 0..prev_prob.len() {
            new_prob[i] += prev_prob[i] * prob;
            new_prob[i + 1] += prev_prob[i] * (1.0 - prob);
        }

        new_prob

    }
}

pub fn get_avg_stddev(times: &[Time]) -> Option<(f64, f64)> {
    let times_weight: Vec<(f64, f64)> = times.iter()
            .filter_map(|t| {
                match t {
                    &Time::DNF => None,
                    &Time::Time(ref x) => Some((*x as f64 / 100f64, 0.01)),
                    &Time::TimeWithDate(ref x, ref d) => {
                        let date_diff = (*d).signed_duration_since(offset::Utc::today()).num_weeks();
                        Some((*x as f64 / 100f64, 1f64 / date_diff as f64))
                    }
                }
            })
            .collect();
    if times_weight.len() == 0 {
        return None;
    }
    let weighted_sum: f64 = times_weight.iter()
            .map(|t| t.0 * t.1)
            .sum();
    let total_weight: f64 = times_weight.iter()
            .map(|t| t.1)
            .sum();
    let avg = weighted_sum / total_weight;
    let weighted_stddev: f64 = times_weight.iter()
            .map(|t| (t.0 - avg) * (t.0 - avg) * t.1)
            .sum();
    Some((avg, f64::sqrt(weighted_stddev / total_weight)))
}

impl WcaResults {
    pub fn ext_person<'a>(&'a self, id: &'a str) -> Option<ExtendedPerson<'a>> {
        self.people.get(id).map(|person|
            ExtendedPerson {
                person: person,
                id: id.to_string(),
                current_avgs: person.get_avgs()
            }
        )
    }

    pub fn comp_info<'a>(&'a self, id: &'a str) -> Option<CompInfo<'a>> {
        let comp = self.comps.get(id);
        match comp {
            None => None,
            Some(&ref comp) => {
                let people: Vec<ExtendedPerson<'a>> = 
                        comp.competitors.iter()
                            .filter_map(|competitor|
                                self.ext_person(&competitor.id)
                            )
                            .collect();

                let mut event_places = HashMap::new();

                for event in comp.events.iter() {
                    let mut places = HashMap::new();

                    let competitors: Vec<_> =
                            comp.competitors.iter()
                            .filter(|p| p.events.iter().find(|x| x == &event).is_some())
                            .filter_map(|p| self.ext_person(&p.id))
                            .collect();

                    for person in people.iter() {
                        match self.ext_person(&person.id) {
                            Some(p) => {
                                places.insert(person.id.clone(),
                                              p.place_prob(competitors.as_slice(), &event)
                                             );

                            }
                            None => {
                                continue;
                            }
                        }
                    }
                    event_places.insert(event.clone(), places);

                }

                Some(CompInfo {comp: comp, people: people, places_chances: event_places})

            }
        }
    }
}

#[derive(Debug, Serialize)]
pub struct Competition {
    pub name: String,
    pub id: String,
    pub events: Vec<String>,
    pub competitors: Vec<Competitor>,
    pub has_been: bool,
    pub start: DateW,
    pub end: DateW,
}

// impl Serialize for Competition {
//     fn serialize<S>(&self, ser: S) -> Result<S::Ok, S::Error>
//         where S: Serializer
//     {
//         let mut s = ser.serialize_struct("Comp", 6)?;
//         s.serialize_field("name", &self.name)?;
//         s.serialize_field("id", &self.id)?;
//         s.serialize_field("events", &self.events)?;
//         s.serialize_field("people", &self.competitors)?;
//         s.serialize_field("start", &*format!("{}", self.start))?;
//         s.serialize_field("end", &*format!("{}", self.end))?;
//         s.end()
//     }
// }

#[derive(Serialize, Debug, Default)]
pub struct Competitor {
    pub id: String,
    pub events: Vec<String>
}

#[derive(Debug)]
pub enum Time {
    DNF,
    TimeWithDate(u16, DateW), // Time with date recorded.
    Time(u16),
}

impl Time {
    fn to_option_sec(&self) -> Option<f64> {
        match *self {
            Time::DNF => None,
            Time::TimeWithDate(t, _) => Some(t as f64 / 100f64),
            Time::Time(t) => Some(t as f64 / 100f64),
        }
    }
}

impl Serialize for Time {
    fn serialize<S>(&self, ser: S) -> Result<S::Ok, S::Error>
        where S: Serializer
    {

        match *self {
            Time::DNF => { ser.serialize_str("DNF") }
            Time::Time(time) => { ser.serialize_f32((time as f32) / 100.0) }
            Time::TimeWithDate(time, _) => { ser.serialize_f32((time as f32) / 100.0) }
        }
    }
}

impl Default for Time {
    fn default() -> Time { Time::DNF }
}

