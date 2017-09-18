#![feature(plugin)]
#![plugin(rocket_codegen)]

extern crate rocket;

use std::io::Cursor;
use std::sync::{Mutex, Arc};
use std::path::{Path, PathBuf};
use std::fs::File;

use rocket::{Request, Response, State};
use rocket::http::{ContentType, Status};

#[derive(Debug, Default)]
struct WebState {
    count: Arc<Mutex<u32>>
}

const HTML_NOT_FOUND: &'static str = include_str!("not_found.html");

fn make_html<'r>(content: String) -> Response<'r> {
    let mut resp = Response::new();
    resp.set_status(Status::Ok);
    resp.set_header(ContentType::HTML);
    resp.set_sized_body(Cursor::new(content));

    resp
}

#[get("/<file..>", rank = 5)]
fn index<'r>(file: PathBuf) -> Option<Response<'r>> {

    let path = Path::new("Static").join(file);
    println!("Path: {:?}", path);

    if let Ok(file) = File::open(&path) {
        println!("Found content");

        let mut resp = Response::new();
        resp.set_status(Status::Ok);
        resp.set_sized_body(file);
        if let Some(ext) = path.extension() {
            if let Some(ext) = ext.to_str() {
                let ctype = ContentType::from_extension(ext);
                if let Some(t) = ctype {
                    resp.set_header(t);
                }
            }
        }

        Some(resp)
    }
    else {
        None
    }
}

#[get("/api/count", rank = 0)]
fn count<'r>(state: State<WebState>) -> Response<'r> {
    let mut count = state.count.lock().unwrap();
    *count += 1;

    make_html(format!("{}", count))
}

#[error(404)]
fn not_found<'r>(_req: &Request) -> Response<'r> {
    make_html(HTML_NOT_FOUND.to_string())
}

fn main() {
    rocket::ignite()
        .manage(WebState::default())
        .mount("/", routes![count, index])
        .catch(errors![not_found])
        .launch();
}
