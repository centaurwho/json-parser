use crate::parse::parse;

mod parse;
mod value;

const JSON_STR: &str = "{\"key1\": {\"inner1\": 0}, \"key2\": [12, 23, 34]}";

fn main() {
    let parsed = parse(JSON_STR);
    println!("{:#?}", parsed);
}


