/*
 * Simple lib to download and parse the WCA results.
 *
 */

extern crate zip;
extern crate reqwest;

use self::zip::read::{ZipArchive, ZipFile};
use self::zip::result::ZipError;
use std::io::{Read, Cursor, BufReader, BufRead};
use std::io;
use std::collections::HashMap;
use std::convert::From;
use std::vec::Vec;

#[derive(Debug)]
pub enum WcaError {
    ZipE(ZipError),
    IoE(io::Error),
    NetE(reqwest::Error),
    ReadE(String),
    PersonE(String)
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
impl From<reqwest::Error> for WcaError {
    fn from(e: reqwest::Error) -> WcaError {
        WcaError::NetE(e)
    }
}


#[derive(Debug, Default)]
pub struct WcaResults {
    pub people: HashMap<String, WcaPerson> // Id: Person
}

#[derive(Debug, Default)]
pub struct WcaPerson {
    pub name: String,
    pub times: HashMap<String, Vec<Time>> // Event: [times]
}

#[derive(Debug)]
pub enum Time {
    DNF,
    Time(u16)
}

impl Default for Time {
    fn default() -> Time { Time::DNF }
}


const WCA_TSV_URL: &'static str = "https://www.worldcubeassociation.org/results/misc/WCA_export.tsv.zip";


// Downloads and parses the current WCA results.
pub fn download_wca<'a>() -> Result<WcaResults, WcaError> {
    let mut resp = reqwest::get(WCA_TSV_URL)?;

    let mut res = Vec::new(); // Resulting ZIP content
    resp.read_to_end(&mut res)?;

    let cur = Cursor::new(res);
    let mut zip = ZipArchive::new(cur)?;

    let file = zip.by_name("WCA_export_Results.tsv")?;

    parse_wca(file)
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

#[cfg(test)]
const TEST_LINE: &'static str = "LyonOpen2007\t333\t1\t15\t1968\t2128\tEtienne Amany\t2007AMAN01\tCote d_Ivoire\ta\t-1\t2203\t2138\t2139\t2108\tAfR\tAfR";

#[test]
pub fn test() {
    let mut res = WcaResults::default();
    println!("Res: {:?}", res);
    assert_eq!(insert_result(TEST_LINE, &mut res).is_ok(), true);
    println!("Res: {:?}", res);
    
}

#[test]
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
