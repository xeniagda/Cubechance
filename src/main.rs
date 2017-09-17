#![feature(plugin)]
#![plugin(rocket_codegen)]

extern crate rocket;

#[get("/")]
fn index() -> String {
    "Hello, world!".to_string()
}

fn main() {
    rocket::ignite().mount("/", routes![index]).launch();
}
