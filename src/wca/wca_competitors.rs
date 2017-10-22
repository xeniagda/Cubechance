
extern crate select;
extern crate reqwest;

use self::select::document::Document;
use self::select::node::Node;
use self::select::predicate as p;

use super::*;


pub fn download_competitors<'a>(comp: &'a str) -> Result<Vec<Competitor>, WcaError> {
    let url = format!("http://www.worldcubeassociation.org/competitions/{}/registrations", comp);
    let resp = reqwest::get(reqwest::Url::parse(&*url)?)?;

    let doc = Document::from_read(resp)?;

    parse_competitors(doc)
}

fn parse_competitors<'a>(doc: Document) -> Result<Vec<Competitor>, WcaError> {
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
    let id   = parse_competitor_id(node).ok_or(WcaError::CompE("No id".to_string()))?;
    let name = parse_competitor_name(node).ok_or(WcaError::CompE("No name".to_string()))?;

    let events = parse_events(node);

    Ok(Competitor{id: id, name: name, events: events})
}

fn parse_events(node: Node) -> Vec<String> {
    node.children()
        .filter(|node| node.name().is_some()) // Remove all whitespace
        .skip(2) // Skip name and country.
        .flat_map(|node| node.find(p::Class("cubing-icon")))
        .filter_map(|event| event.attr("class")) // Extract the class
        .filter_map(|class| class.split("-").last()) // Class is in form cubing-icon event-xxx, we extract the xxx
        .map(|event| event.to_string())
        .collect()
}

fn parse_competitor_id(node: Node) -> Option<String> {
    node.find(p::Name("a")) // The wca ID is stored as a link, find it,
        .filter_map(|tag| tag.attr("href")) // get the href
        .nth(0) // Aquire the link
        .and_then(|id| id.split("/").nth(2)) // The link is in format /persons/<id>, so we split on / and take the third thing
        .map(|id| id.to_string()) // and make it into a String
}

fn parse_competitor_name(node: Node) -> Option<String> {
    node.find(p::Name("a")) // The wca ID is stored as a link, find it,
        .nth(0)
        .map(|name| name.text())
}
