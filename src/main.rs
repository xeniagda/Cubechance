#![feature(plugin)]
#![plugin(rocket_codegen)]

extern crate rocket;

use std::io::Cursor;
use std::sync::{Mutex, Arc};

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

#[get("/")]
fn index<'r>(state: State<WebState>) -> Response<'r> {
    let mut count = state.count.lock().unwrap();
    *count += 1;

    make_html(format!("main page {}", count))
}

#[error(404)]
fn not_found<'r>(_req: &Request) -> Response<'r> {
    make_html(HTML_NOT_FOUND.to_string())
}

fn main() {
    rocket::ignite()
        .manage(WebState::default())
        .mount("/", routes![index])
        .catch(errors![not_found])
        .launch();
}
