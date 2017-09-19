/*
 * Simple lib to download and parse the WCA results.
 *
 */

extern crate zip;
extern crate reqwest;

use self::zip::read::ZipArchive;
use std::io::{Read, Cursor};


const WCA_TSV_URL: &'static str = "https://www.worldcubeassociation.org/results/misc/WCA_export.tsv.zip";

// Downloads and parses the current WCA results.
pub fn download_wca() {
    println!("Downloading wca...");

    match reqwest::get(WCA_TSV_URL) {
        Err(e) => {
            eprintln!("Couldn't download wca: {}", e);
        }

        Ok(ref mut resp) => {
            let mut res = Vec::new(); // Resulting ZIP content
            if let Err(e) = resp.read_to_end(&mut res) {
                println!("Couldn't read to string: {}", e);
                return;
            }
            println!("Downloaded. Unzipping...");
            
            match ZipArchive::new(Cursor::new(res)) {
                Ok(ref mut zip) => {
                    match zip.by_name("WCA_export_Results.tsv") {
                        Ok(ref mut file) => {
                            let mut content = String::new();
                            if let Err(e) = file.read_to_string(&mut content) {
                                println!("Could'nt read file: {:?}", e);
                            }

                            println!("Found file. Size: {}", content.len());
                        }
                        Err(e) => {
                            println!("Error reading zip: {:?}", e);
                        }
                    }
                }
                Err(e) => {
                    println!("Error reading zip: {:?}", e);
                }
            }
        }
    }
}