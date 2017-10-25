/*
 * Simple lib to download and parse the WCA results.
 *
 */

extern crate zip;
extern crate reqwest;
extern crate chrono;
extern crate rayon;

use self::zip::read::{ZipArchive, ZipFile};
use std::io::{Read, Cursor, BufReader, BufRead};
use std::vec::Vec;
use std::sync::Arc;
use std::sync::Mutex;

use self::rayon::iter::IntoParallelIterator;
use self::rayon::iter::ParallelIterator;

use self::chrono::{Date, NaiveDate, offset};

use super::*;

const WCA_TSV_URL: &'static str = "https://www.worldcubeassociation.org/results/misc/WCA_export.tsv.zip";


// Two default comps when you compile with skip_comps
#[cfg(any(skip_comps, test))]
const SKILLCON: &'static str = "Skillcon2017\tSkillcon 2017\tLas Vegas, Nevada\tUSA\tThis is a WCA competition presented in conjunction with SkillCon 2017. Registration fees will cover admission for SkillCon; check the [SkillCon website](http://skillcon.org/) for more info on the other events and seminars that will be going on. Registration will cost a flat $50 per competitor. **Before registering, you must read the \"Competitor Responsibilities\" tab.** This tab details the responsibilites of all competitors at this competition.\t2017\t12\t16\t12\t17\t222 333 333bf 333mbf 333oh 444 444bf 555 555bf 666 minx pyram\t[{Shelley Chang}{mailto:shelley.chang@cubingusa.org}] [{Kit Clement}{mailto:kit@cubingusa.org}]\t[{Shelley Chang}{mailto:shelley.chang@cubingusa.org}] [{Kit Clement}{mailto:kit@cubingusa.org}] [{Ryan Jew}{mailto:ryan@icnc.com}]\t[Rio Hotel and Casino](https://www.caesars.com/rio-las-vegas)\t3700 W. Flamingo Road Las Vegas, NV 89103\t\t\tSkillcon 2017\t36117521\t-115188177";
#[cfg(any(skip_comps, test))]
const SSL: &'static str = "SSL2Stockholm2017	SSL 2 Stockholm 2017	Stockholm	Sweden	The competition is a part of the Swedish Speedcubing League 2017. This is the second out of four competitions, followed by a final competiton later this year. The entry fee is set to 150 SEK and must be payed in advance by swedish competitors. Foreigners may pay at the competition venue. A competitor limit has been set to 100 competitors due to venue constraints. More information and payment details can be found on the competition website.	2017	4	8	4	9	222 333 333bf 333fm 333mbf 333oh 444 444bf 555 555bf 666 777 pyram skewb	[{Kåre Krig}{mailto:karekrig@gmail.com}] [{Anders Berggren}{mailto:anders_berggren-sjoblom@hotmail.com}]	[{Daniel Wallin}{mailto:danne_wallain@live.se}] [{Timothy Edegran Gren}{mailto:timothy.edegran@edu.nacka.se}]	Nacka Gymnasium	Griffelvägen 17, 131 40, Nacka	The main hall will be the school dining hall of Nacka Gymnasium. Long events will be held in a side-room close to the ma	http://ssl-se.webnode.se/ssl-2-stockholm-2017/	SSL 2 Stockholm 2017	59310982	18150448";

// Downloads and parses the current WCA results.
pub fn download_wca<'a>() -> Result<WcaResults, WcaError> {
    println!("Loading zip");

    let mut resp = reqwest::get(WCA_TSV_URL)?;

    let mut res = Vec::new(); // Resulting ZIP content
    resp.read_to_end(&mut res)?;

    let cur = Cursor::new(res);
    let mut zip = ZipArchive::new(cur)?;

 
    let mut results = WcaResults::default();// { people: HashMap::new(), comps: HashMap::new(), download_date: DateW::new(offset::Utc::today()) };

    #[cfg(any(skip_comps, test))]
    {
        println!("Skipping comps, adding SkillCon!");
        insert_comp(SSL.to_string(), &mut results)?;
        insert_comp(SKILLCON.to_string(), &mut results)?;
    }
    #[cfg(not(any(skip_comps, test)))]
    {
        println!("Parsing comps...");
        parse_wca_comps(zip.by_name("WCA_export_Competitions.tsv")?, &mut results)?;
    }

    println!("Parsing results...");
    parse_wca_results(zip.by_name("WCA_export_Results.tsv")?, &mut results)?;


    println!("Done");

    Ok(results)
}

pub fn parse_wca_results<'a>(file: ZipFile, mut results: &mut WcaResults) -> Result<(), WcaError> {
    let mut reader = BufReader::new(file);

    let mut i = 0;
    let mut _fl = String::new(); // First line
    if let Err(e) = reader.read_line(&mut _fl) { // Skip the first line
        return Err(WcaError::ReadE(format!("Error reading file: {:?}", e)));
    }

    loop {
        let mut line = String::new();
        i += 1;
        match reader.read_line(&mut line) {
            Ok(_) => {
                if line == "" {
                    break;
                }

                if i % 10000 == 0 {
                    println!("Line {}: {}", i, line);
                }

                if let Err(e) = insert_result(&line, &mut results) {
                    eprintln!("Error on line {}: {:?}", i, e);
                }
            }
            Err(e) => {
                return Err(WcaError::ReadE(format!("Error reading line {}: {:?}", i, e)));
            }
        }
    }

    Ok(())
}

pub fn parse_wca_comps<'a>(file: ZipFile, results: &mut WcaResults) -> Result<(), WcaError> {
    let mut reader = BufReader::new(file);

    let mut _fl = String::new();
    if let Err(e) = reader.read_line(&mut _fl) { // Skip the first line
        return Err(WcaError::ReadE(format!("Error reading file: {:?}", e)));
    }

    let lines: Vec<_> =
            reader.lines().collect();

    let comp_amut = Arc::new(Mutex::new(results));

    lines.into_par_iter()
        .map(|line| load_comp(line.unwrap()).unwrap())
        .for_each(|comp| {
            let mut res = comp_amut.lock().unwrap();
            res.comps.insert(comp.id.clone(), comp);
        });

    Ok(())
}

pub fn insert_result<'a>(line: &'a str, results: &mut WcaResults) -> Result<(), WcaError> {
    let mut stuff = Vec::new();
    for segment in line.split("\t") {
        stuff.push(segment);
    }
    if stuff.len() != 17 {
        return Err(WcaError::PersonE(format!("Expected 17 values, found {}", stuff.len())));
    }

    let comp_id = stuff[0];
    let event = stuff[1];
    let name = stuff[6];
    let id = stuff[7];
    let mut times = Vec::new();

    for i in 10..15 {
        let time: Result<i32, _> = stuff[i].parse();
        match time {
            Ok(x) => {
                if x < 0 {
                    times.push(Time::DNF);
                }
                else if x != 0 {
                    let comp = results.comps.get(comp_id);
                    match comp {
                        None => { times.push(Time::Time(x as u16)); }
                        Some(&ref comp) => {
                            times.push(Time::TimeWithDate(x as u16, comp.start.clone()));
                        }
                    }
                }
            }
            Err(e) => {
                return Err(WcaError::PersonE(format!("Couldn't parse: {:?}", e)));
            }
        }
    }

    let person = results.people.entry(id.to_string()).or_insert(WcaPerson::default());

    (*person).name = name.to_string();
    let p_times = (*person).times.entry(event.to_string()).or_insert(Vec::new());

    p_times.append(&mut times);

    Ok(())
}

pub fn load_comp<'a>(line: String) -> Result<Competition, WcaError> {
    let mut stuff = Vec::new();
    for segment in line.split("\t") {
        stuff.push(segment);
    }
    if stuff.len() != 20 {
        return Err(WcaError::PersonE(format!("Expected 20 values, found {}", stuff.len())));
    }
    let comp_id = stuff[0];
    let comp_name = stuff[1];

    // Start date
    let s_year = stuff[5];
    let s_month = stuff[6];
    let s_day = stuff[7];
    let s_n_date = NaiveDate::from_ymd(s_year.parse()?, s_month.parse()?, s_day.parse()?);
    let s_date = DateW::new(Date::from_utc(s_n_date, offset::Utc));

    // End date
    let e_month = stuff[8];
    let e_day = stuff[9];
    let e_n_date = NaiveDate::from_ymd(s_year.parse()?, e_month.parse()?, e_day.parse()?);
    let e_date = DateW::new(Date::from_utc(e_n_date, offset::Utc));

    let events = stuff[10].split(" ").map(|s| s.to_string()).collect();

    let has_been = *s_date < offset::Utc::today();


    let comp =
            if has_been {
                Competition {
                    name: comp_name.to_string(),
                    id: comp_id.to_string(),
                    events: events,
                    has_been: has_been,
                    start: s_date,
                    end: e_date,
                    competitors: vec![]
                }
            } else {
                println!("Downloading {}", comp_name);
                Competition {
                    name: comp_name.to_string(),
                    id: comp_id.to_string(),
                    events: events,
                    has_been: has_been,
                    start: s_date,
                    end: e_date,
                    competitors: wca_competitors::download_competitors(comp_id)?
                }
            };

    Ok(comp)

}
