/*
 * Simple lib to download and parse the WCA results.
 *
 */

extern crate zip;
extern crate reqwest;
extern crate chrono;


use self::zip::read::{ZipArchive, ZipFile};
use std::io::{Read, Cursor, BufReader, BufRead};
use std::vec::Vec;

use self::chrono::{Date, NaiveDate, offset};

use super::*;

const WCA_TSV_URL: &'static str = "https://www.worldcubeassociation.org/results/misc/WCA_export.tsv.zip";


#[cfg(skip_comps)]
const SKILLCON: &'static str = "Skillcon2017\tSkillcon 2017\tLas Vegas, Nevada\tUSA\tThis is a WCA competition presented in conjunction with SkillCon 2017. Registration fees will cover admission for SkillCon; check the [SkillCon website](http://skillcon.org/) for more info on the other events and seminars that will be going on. Registration will cost a flat $50 per competitor. **Before registering, you must read the \"Competitor Responsibilities\" tab.** This tab details the responsibilites of all competitors at this competition.\t2017\t12\t16\t12\t17\t222 333 333bf 333mbf 333oh 444 444bf 555 555bf 666 minx pyram\t[{Shelley Chang}{mailto:shelley.chang@cubingusa.org}] [{Kit Clement}{mailto:kit@cubingusa.org}]\t[{Shelley Chang}{mailto:shelley.chang@cubingusa.org}] [{Kit Clement}{mailto:kit@cubingusa.org}] [{Ryan Jew}{mailto:ryan@icnc.com}]\t[Rio Hotel and Casino](https://www.caesars.com/rio-las-vegas)\t3700 W. Flamingo Road Las Vegas, NV 89103\t\t\tSkillcon 2017\t36117521\t-115188177";

// Downloads and parses the current WCA results.
pub fn download_wca<'a>() -> Result<WcaResults, WcaError> {
    println!("Loading zip");

    let mut resp = reqwest::get(WCA_TSV_URL)?;

    let mut res = Vec::new(); // Resulting ZIP content
    resp.read_to_end(&mut res)?;

    let cur = Cursor::new(res);
    let mut zip = ZipArchive::new(cur)?;

 
    let mut results = WcaResults::default();

    println!("Skip comps: {}", cfg!(skip_comps));

    #[cfg(skip_comps)]
    {
        println!("Skipping comps, adding SkillCon!");
        insert_comp(SKILLCON, &mut results)?;
    }
    #[cfg(not(skip_comps))]
    {
        println!("Parsing comps...");
        parse_wca_comps(zip.by_name("WCA_export_Competitions.tsv")?, &mut results)?;
    }

    println!("Parsing results...");
    parse_wca_results(zip.by_name("WCA_export_Results.tsv")?, &mut results)?;

    println!("Done");

    Ok(results)
}

fn parse_wca_results<'a>(file: ZipFile, mut results: &mut WcaResults) -> Result<(), WcaError> {
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

fn parse_wca_comps<'a>(file: ZipFile, mut results: &mut WcaResults) -> Result<(), WcaError> {
    let mut reader = BufReader::new(file);

    let mut i = 0;
    let mut _fl = String::new();
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

                if let Err(e) = insert_comp(&line, &mut results) {
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

fn insert_result<'a>(line: &'a str, results: &mut WcaResults) -> Result<(), WcaError> {
    let mut stuff = Vec::new();
    for segment in line.split("\t") {
        stuff.push(segment);
    }
    if stuff.len() != 17 {
        return Err(WcaError::PersonE(format!("Expected 17 values, found {}", stuff.len())));
    }

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
                    times.push(Time::Time(x as u16));
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

fn insert_comp<'a>(line: &'a str, results: &mut WcaResults) -> Result<(), WcaError> {
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
    let s_date: Date<offset::Utc> = Date::from_utc(s_n_date, offset::Utc);

    // End date
    let e_year = stuff[5];
    let e_month = stuff[6];
    let e_day = stuff[7];
    let e_n_date = NaiveDate::from_ymd(e_year.parse()?, e_month.parse()?, e_day.parse()?);
    let e_date: Date<offset::Utc> = Date::from_utc(e_n_date, offset::Utc);

    let events = stuff[8].split(" ").map(|e| e.to_string()).collect();

    let has_been = s_date < offset::Utc::today();


    let comp =
            if has_been {
                Competition {
                    name: comp_name.to_string(),
                    id: comp_id.to_string(),
                    events: events,
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
                    start: s_date,
                    end: e_date,
                    competitors: wca_competitors::download_competitors(comp_id)?
                }
            };
    results.comps.push(comp);

    Ok(())

}


#[cfg(test)]
const TEST_RESULT_LINE: &'static str = "LyonOpen2007\t333\t1\t15\t1968\t2128\tEtienne Amany\t2007AMAN01\tCote d_Ivoire\ta\t-1\t2203\t2138\t2139\t2108\tAfR\tAfR";
#[test]
pub fn test_result() {
    let mut wca = WcaResults::default();
    println!("Before: {:?}", wca);
    assert_eq!(insert_result(TEST_RESULT_LINE, &mut wca).is_ok(), true);
    println!("Res: {:?}", wca);
}

#[cfg(test)]
const TEST_COMP_LINE: &'static str = "ArenaCurucaOpen2018\tArena Curuçá Open 2018\tSão Paulo - SP\tBrazil\tA inscrição é **gratuita** e aberta a qualquer pessoa de qualquer nacionalidade. As inscrições para todas as modalidades poderão ser feitas até o dia 21 de janeiro de 2018. No dia do campeonato, as inscrições estarão abertas somente para o 3x3. Mais informações na aba \"Inscrições\".\t2016\t1\t27\t1\t27\t222 333 pyram\t[{Ronan Felipe Jorge}{mailto:ronan.jorge@hotmail.com}]\t[{Mauricio Paulino Marques Fernandes}{mailto:mauriciopmf@yahoo.com.br}]\t[Arena Curuçá](http://www.curucafutsal.com.br)\tRua Grapira, 70 - Vila Curuçá, São Miguel Paulista\t\thttps://sites.google.com/prod/view/arenaopen\tArena Curuçá Open 2018\t-23496048\t-46422484";
#[test]
pub fn test_comp() {
    let mut wca = WcaResults::default();
    if let Err(e) = insert_comp(TEST_COMP_LINE, &mut wca) {
        println!("Error: {:?}", e);
        assert!(false);
    }
    assert_eq!(wca.comps.len(), 1);
    assert_eq!(wca.comps[0].id, "ArenaCurucaOpen2018");
    assert_eq!(wca.comps[0].name, "Arena Curuçá Open 2018");
    println!("Comp: {:?}", wca.comps[0]);
}

// #[cfg(test)]
// use std::fs::File;
// #[cfg(test)]
// use std::io::Write;

// #[test]
// pub fn test_download() {
//     let wca = download_wca();

//     match wca {
//         Ok(wca) => {
//             println!("Me: {:?}", wca.people.get("2015LOOV01"));
//             let mut file = File::create("Wca.obj");
//             match file {
//                 Ok(ref mut file) => {
//                     file.write_all(format!("{:?}", wca).as_bytes()).expect("Couldn't write!");
//                 }
//                 Err(e) => {
//                     eprintln!("Error writing: {:?}", e);
//                 }
//             }
//         }
//         Err(e) => {
//             eprintln!("Something failed: {:?}", e);
//             assert!(false);
//         }
//     }

// }
