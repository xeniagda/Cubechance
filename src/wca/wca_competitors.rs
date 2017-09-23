
extern crate select;
extern crate reqwest;

use std::io::{Read};
use self::select::document::Document;
use self::select::node::Node;
use self::select::predicate as p;

use super::*;


fn download_html<'a>(comp: &'a str) -> Result<(), WcaError> {
    let url = format!("http://www.worldcubeassociation.org/competitions/{}/registrations", comp);
    let resp = reqwest::get(reqwest::Url::parse(&*url)?)?;

    let doc = Document::from_read(resp)?;

    find_comp_table(doc)?;

    Ok(())
}

fn find_comp_table<'a>(doc: Document) -> Result<Vec<Competitor>, WcaError> {
    let table =
            doc.find(p::Class("wca-results"))
            .filter_map(|table| table.children().nth(3)) // The third child is the competitors table
            .flat_map(|table| table.children()) // Get all competitors
            .filter(|node| node.name().is_some()) // Remove all whitespace
            .filter_map(|node| parse_competitor(node).ok())
            .collect();
    Ok(table)
}

fn parse_competitor(node: Node) -> Result<Competitor, WcaError> {
    let id = parse_competitor_id(node).ok_or(WcaError::CompE("No id".to_string()))?;
    println!("Id: {}", id);

    let events = parse_events(node).ok_or(WcaError::CompE("No events".to_string()))?;
    println!("Events: {:?}", events);

    Ok(Competitor{id: id, events: events})
}

fn parse_events(node: Node) -> Option<Vec<String>> {
    Some(vec![])
}

fn parse_competitor_id(node: Node) -> Option<String> {
    node.find(p::Name("a")) // The wca ID is stored as a link, find it,
        .filter_map(|tag| tag.attr("href")) // get the href
        .nth(0) // Aquire the link
        .and_then(|id| id.split("/").nth(2)) // The link is in format /persons/<id>, so we split on / and take the third thing
        .map(|id| id.to_string()) // and make it into a String
}

#[test]
fn test_download() {
    if let Err(e) = download_html("SkillCon2017") {
        eprintln!("Error: {:?}", e);
        assert!(false);
    }
}
