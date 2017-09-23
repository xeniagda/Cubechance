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

use self::chrono::{Duration, Date, NaiveDate, offset};

use super::*;


const WCA_TSV_URL: &'static str = "https://www.worldcubeassociation.org/results/misc/WCA_export.tsv.zip";


// Downloads and parses the current WCA results.
pub fn download_wca<'a>() -> Result<WcaResults, WcaError> {
    let mut resp = reqwest::get(WCA_TSV_URL)?;

    let mut res = Vec::new(); // Resulting ZIP content
    resp.read_to_end(&mut res)?;

    let cur = Cursor::new(res);
    let mut zip = ZipArchive::new(cur)?;

    let results = zip.by_name("WCA_export_Results.tsv")?;

    parse_wca(results)
}

fn parse_wca<'a>(file: ZipFile) -> Result<WcaResults, WcaError> {
    let mut reader = BufReader::new(file);

    let mut i = 0;
    let mut _fl = String::new(); // First line
    if let Err(e) = reader.read_line(&mut _fl) { // Skip the first line
        return Err(WcaError::ReadE(format!("Error reading file: {:?}", e)));
    }
 
    let mut results = WcaResults::default();

    loop {
        let mut line = String::new();
        i += 1;
        match reader.read_line(&mut line) {
            Ok(_) => {
                if line == "" {
                    break;
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

    Ok(results)
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

    let time_left = offset::Utc::today().signed_duration_since(s_date);
    let has_been = time_left < Duration::zero();

    println!("Time left: {}", time_left);


    let comp = 
            if has_been {
                Competition {
                    name: comp_name.to_string(),
                    id: comp_id.to_string(),
                    start: s_date,
                    end: e_date,
                    competitors: vec![]
                }
            } else {
                Competition {
                    name: comp_name.to_string(),
                    id: comp_id.to_string(),
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

#[cfg(test)]
use std::fs::File;
use std::io::Write;

//#[test]
pub fn test_download() {
    let wca = download_wca();

    match wca {
        Ok(wca) => {
            println!("Me: {:?}", wca.people.get("2015LOOV01"));
            let mut file = File::create("Wca.obj");
            match file {
                Ok(ref mut file) => {
                    file.write_all(format!("{:?}", wca).as_bytes()).expect("Couldn't write!");
                }
                Err(e) => {
                    eprintln!("Error writing: {:?}", e);
                }
            }
        }
        Err(e) => {
            eprintln!("Something failed: {:?}", e);
            assert!(false);
        }
    }

}
