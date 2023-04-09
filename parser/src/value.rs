use std::collections::HashMap;

// TODO: Visibility of these structs and their fields is all over the place. Go over all of them
//  and decide the best option

#[derive(Debug, Eq, PartialEq)]
pub struct Json {
    element: JsonElement,
}

impl Json {
    pub fn new(value: JsonValue) -> Json {
        Json {
            element: JsonElement::new(value),
        }
    }
}

#[derive(Debug, Eq, PartialEq)]
pub struct JsonElement {
    pub value: JsonValue,
}

impl JsonElement {
    pub fn new(value: JsonValue) -> JsonElement {
        JsonElement { value }
    }
}

#[derive(Debug, Eq, PartialEq)]
pub enum JsonValue {
    JsonObject(HashMap<String, JsonElement>),
    JsonArray(Vec<JsonElement>),
    JsonString(String),
    Number(Number),
    Boolean(bool),
    Null,
}

#[derive(Debug, Eq, PartialEq)]
pub struct Hex(pub char);

#[derive(Debug, Eq, PartialEq)]
pub struct Number {
    integer: i32,
    fraction: Option<Fraction>,
    exponent: Option<Exponent>,
}

impl Number {
    pub fn new(integer: i32, fraction: Option<Fraction>, exponent: Option<Exponent>) -> Number {
        Number {
            integer,
            fraction,
            exponent,
        }
    }
}

// TODO: Change this representation. Storing this as f32 could make more sense. Then the problem is
//  precision. It may be better to do the conversion when needed.
#[derive(Debug, Eq, PartialEq)]
pub struct Fraction(pub u32);

#[derive(Debug, Eq, PartialEq)]
pub struct Exponent {
    sign: Sign,
    val: u32,
}

impl Exponent {
    pub fn new(sign: Sign, val: u32) -> Exponent {
        Exponent { sign, val }
    }
}

#[derive(Debug, Eq, PartialEq, Copy, Clone)]
pub enum Sign {
    Plus,
    Minus,
}
