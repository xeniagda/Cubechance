extern crate zip;
extern crate reqwest;
extern crate chrono;
extern crate serde;

pub mod wca_export;
pub mod wca_competitors;

use self::serde::{Serialize, Serializer};

use self::zip::result::ZipError;
use std::num::ParseIntError;
use std::io;
use std::collections::HashMap;
use std::convert::From;
use std::vec::Vec;
use std::ops::Deref;

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


#[derive(Serialize, Debug)]
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

#[derive(Serialize)]
pub struct CompInfo<'a> {
    pub comp: &'a Competition,
    pub people: Vec<ExtendedPerson<'a>>
}

#[derive(Serialize)]
pub struct ExtendedPerson<'a> {
    pub person: &'a WcaPerson,
    pub id: String,
    pub current_avgs: HashMap<String, Time>, // Event: avg
}

impl WcaPerson {
    pub fn get_avgs(&self) -> HashMap<String, Time> {
        let mut res = HashMap::new();
        for (event, times) in (*self).times.iter() {
            let avg: f64 = times.iter()
                        .filter_map(|t| t.to_option_sec())
                        .map(|t| t / (times.len() as f64))
                        .sum();
            res.insert(event.clone(), Time::Time((avg * 100f64) as u16));
        }
        res
    }
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

                Some(CompInfo {comp: comp, people: people})

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

