#![feature(plugin)]
#![plugin(rocket_codegen)]

extern crate rocket;

use std::io::Cursor;
use std::sync::{Mutex, Arc};
use std::path::{Path, PathBuf};
use std::fs::File;
use std::thread;

use rocket::{Request, Response, State};
use rocket::http::{ContentType, Status};

mod wca;

#[derive(Debug, Default)]
struct WebState {
    wca: Option<wca::WcaResults>
}

type MutWebState = Arc<Mutex<WebState>>;

const HTML_NOT_FOUND: &'static str = include_str!("not_found.html");

fn make_html<'r>(content: String) -> Response<'r> {
    let mut resp = Response::new();
    resp.set_status(Status::Ok);
    resp.set_header(ContentType::HTML);
    resp.set_sized_body(Cursor::new(content));

    resp
}

#[get("/", rank=1)]
fn index_<'r>() -> Option<Response<'r>> {
    let mut path = PathBuf::new();
    path.push("index.html");
    index(path)
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

#[get("/api/wca/<id>", rank = 0)]
fn count<'r>(id: String, state: State<MutWebState>) -> Response<'r> {
    let state = state.lock().unwrap();

    match state.wca {
        Some(ref wca) => {
            make_html(format!("{:?}", wca.people.get(&id)))
        }
        None => {
            make_html("wait".to_string())
        }
    }
}

#[error(404)]
fn not_found<'r>(_req: &Request) -> Response<'r> {
    make_html(HTML_NOT_FOUND.to_string())
}

fn main() {
    // Setup WCA reading in background
    //
    let state = MutWebState::default();

    let thread_state = state.clone();
    thread::spawn(move || {
        loop {
            println!("Downloading wca...");
            let comp = wca::download_wca().expect("No compressed data!");
            let mut state = thread_state.lock().unwrap();
            state.wca = Some(comp);
        }
    });

    rocket::ignite()
        .manage(state)
        .mount("/", routes![count, index, index_])
        .catch(errors![not_found])
        .launch();
}
