#![feature(plugin)]
#![plugin(rocket_codegen)]

extern crate rocket;
extern crate serde_json;

#[cfg(test)]
#[macro_use]
extern crate lazy_static; // For testing
#[cfg(test)]
mod test;

#[macro_use]
extern crate serde_derive;

use std::io::Cursor;
use std::sync::{Mutex, Arc};
use std::path::{Path, PathBuf};
use std::fs::File;
use std::thread;
use std::time::{Duration, Instant};

use rocket::{Request, Response, State};
use rocket::http::{ContentType, Status};

mod wca;
use wca::wca_export;

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

    if let Ok(file) = File::open(&path) {
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

#[get("/api/people/<name>", rank = 0)]
fn wca_person<'r>(name: String, state: State<MutWebState>) -> Response<'r> {
    let state = state.lock().unwrap();

    match state.wca {
        Some(ref wca) => {
            let people: Vec<_> = wca.people.iter()
                    .filter(|&(_, p)| p.name.contains(&name))
                    .filter_map(|(id, _)| wca.ext_person(&id))
                    .collect();


            if people.len() < 200 {
                match serde_json::to_string(&people) {
                    Ok(json) => {
                        make_html(json)
                    }
                    Err(e) => {
                        make_html(format!("e2 {}", e))
                    }
                }
            }
            else {
                make_html("e3".to_string())
            }
        }
        None => {
            make_html("e0".to_string())
        }
    }
}

#[get("/api/wca/<id>", rank = 0)]
fn wca_id<'r>(id: String, state: State<MutWebState>) -> Response<'r> {
    let state = state.lock().unwrap();

    match state.wca {
        Some(ref wca) => {
            let person = wca.ext_person(&id);
            match person {
                Some(person) => {
                    match serde_json::to_string(&person) {
                        Ok(json) => {
                            make_html(json)
                        }
                        Err(e) => {
                            make_html(format!("e2 {}", e))
                        }
                    }
                }
                None => {
                    make_html("e1".to_string())
                }
            }
        }
        None => {
            make_html("e0".to_string())
        }
    }
}

#[get("/api/upcoming")]
fn upcoming<'r>(state: State<MutWebState>) -> Response<'r> {
    let state = state.lock().unwrap();
    match state.wca {
        Some(ref wca) => {
            let comps: Vec<&wca::Competition> = wca.comps.values()
                    .filter(|comp| !comp.has_been)
                    .collect();
            match serde_json::to_string(&comps) {
                Ok(json) => {
                    make_html(json)
                }
                Err(e) => {
                    make_html(format!("e2 {}", e))
                }
            }
        }
        None => {
            make_html("e0".to_string())
        }
    }
}

#[get("/api/beat/<id1>/<id2>/<event>", rank = 0)]
fn beating<'r>(id1: String, id2: String, event: String, state: State<MutWebState>) -> Response<'r> {
    let state = state.lock().unwrap();
    match state.wca {
        Some(ref wca) => {
            let p1 = wca.ext_person(&id1);
            let p2 = wca.ext_person(&id2);
            match (p1, p2) {
                (Some(ref p1), Some(ref p2)) => {
                    make_html(format!("{:?}", p1.chance_beating(p2, &event)))
                }
                _ => {
                    make_html("e1".to_string())
                }
            }
        }
        None => {
            make_html("e0".to_string())
        }
    }
}

#[get("/api/place/<comp>/<id>/<event>", rank = 0)]
fn place<'r>(comp: String, id: String, event: String, state: State<MutWebState>) -> Response<'r> {
    let state = state.lock().unwrap();
    match state.wca {
        Some(ref wca) => {
            match (wca.ext_person(&id), wca.comps.get(&comp)) {
                ( Some(ref person), Some(ref comp) ) => {
                    let competitors: Vec<_> = 
                            comp.competitors.iter()
                            .filter(|p| p.id != id && p.events.iter().any(|e| e == &event))
                            .filter_map(|p| wca.ext_person(&p.id))
                            .collect();
                    let res = person.place_prob(competitors.as_slice(), &event);
                    make_html(format!("{:?}", res))
                }
                _ => {
                    make_html("e1".to_string())
                }
            }
        }
        None => {
            make_html("e0".to_string())
        }
    }
}

#[get("/api/comp/<id>", rank = 0)]
fn comp<'r>(id: String, state: State<MutWebState>) -> Response<'r> {
    let state = state.lock().unwrap();
    match state.wca {
        Some(ref wca) => {
            let comp = wca.comps.values()
                        .filter(|comp| comp.id == id)
                        .nth(0);

            match comp {
                Some(comp) => {

                    match serde_json::to_string(&wca.comp_info(&comp.id)) {
                        Ok(json) => {
                            make_html(json)
                        }
                        Err(e) => {
                            make_html(format!("e2 {}", e))
                        }
                    }
                }
                None => {
                    make_html("e1".to_string())
                }
            }
        }
        None => {
            make_html("e0".to_string())
        }
    }

}

#[error(404)]
fn not_found<'r>(_req: &Request) -> Response<'r> {
    make_html(HTML_NOT_FOUND.to_string())
}

fn main() {
    // Setup WCA reading in background
    let state = MutWebState::default();

    let thread_state = state.clone();
    thread::spawn(move || {
        loop {
            println!("Downloading wca...");
            let start = Instant::now();

            let comp = wca_export::download_wca();

            println!("Downloaded the WCA in {} seconds", start.elapsed().as_secs());

            match comp {
                Ok(comp) => {
                    let mut state = thread_state.lock().unwrap();
                    state.wca = Some(comp);
                }
                Err(e) => {
                    println!("Compressed download failed! Error: {:?}", e);
                }
            }
            thread::sleep(Duration::new(3600, 0)); // Sleep for an hour
        }
    });

    rocket::ignite()
        .manage(state)
        .mount("/", routes![wca_id, wca_person, upcoming, comp, beating, place, index, index_])
        .catch(errors![not_found])
        .launch();
}
