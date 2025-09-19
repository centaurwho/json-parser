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

    pub fn element(&self) -> &JsonElement {
        &self.element
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

    pub fn value(&self) -> &JsonValue {
        &self.value
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

    pub fn integer(&self) -> i32 {
        self.integer
    }

    pub fn fraction(&self) -> Option<&Fraction> {
        self.fraction.as_ref()
    }

    pub fn exponent(&self) -> Option<&Exponent> {
        self.exponent.as_ref()
    }

    pub fn get_value(&self) -> f64 {
        let mut value = self.integer as f64;
        if let Some(fraction) = &self.fraction {
            value += fraction.to_f64();
        }
        if let Some(exponent) = &self.exponent {
            value *= exponent.to_f64();
        }
        value
    }
}

// TODO: Change this representation. Storing this as f32 could make more sense. Then the problem is
//  precision. It may be better to do the conversion when needed.
#[derive(Debug, Eq, PartialEq)]
pub struct Fraction(pub u32);

impl Fraction {
    pub fn new(val: u32) -> Fraction {
        Fraction(val)
    }

    pub fn value(&self) -> u32 {
        self.0
    }

    pub fn to_f64(&self) -> f64 {
        let frac_str = format!("{}", self.0);
        let frac_len = frac_str.len() as u32;
        self.0 as f64 / 10f64.powi(frac_len as i32)
    }
}

#[derive(Debug, Eq, PartialEq)]
pub struct Exponent {
    sign: Sign,
    val: u32,
}

impl Exponent {
    pub fn new(sign: Sign, val: u32) -> Exponent {
        Exponent { sign, val }
    }

    pub fn sign(&self) -> Sign {
        self.sign
    }

    pub fn to_f64(&self) -> f64 {
        match self.sign {
            Sign::Plus => 10f64.powi(self.val as i32),
            Sign::Minus => 10f64.powi(-(self.val as i32)),
        }
    }
}

#[derive(Debug, Eq, PartialEq, Copy, Clone)]
pub enum Sign {
    Plus,
    Minus,
}
