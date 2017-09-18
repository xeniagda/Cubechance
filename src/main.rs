#![feature(plugin)]
#![plugin(rocket_codegen)]

extern crate rocket;

use std::io::Cursor;

use rocket::Request;
use rocket::Response;
use rocket::http::{ContentType, Status};

const HTML_NOT_FOUND: &'static str = include_str!("not_found.html");

fn make_html<'r>(content: &'r str) -> Response<'r> {
    let mut resp = Response::new();
    resp.set_status(Status::Ok);
    resp.set_header(ContentType::HTML);
    resp.set_sized_body(Cursor::new(content));

    resp
}

#[get("/")]
fn index<'r>() -> Response<'r> {
    make_html("main page")
}

#[error(404)]
fn not_found<'r>(_req: &Request) -> Response<'r> {
    make_html(HTML_NOT_FOUND)
}

fn main() {
    rocket::ignite()
        .mount("/", routes![index])
        .catch(errors![not_found])
        .launch();
}
