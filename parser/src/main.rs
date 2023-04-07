use crate::lib::parse;

mod lib;

const JSON_STR: &str = "{\"key1\": {\"key2\": 0}}";

fn main() {
    let parsed = parse(JSON_STR);
    println!("{:?}", parsed);
}


