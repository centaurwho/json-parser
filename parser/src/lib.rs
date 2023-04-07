use nom::branch::alt;
use nom::bytes::complete::{tag, take_while};
use nom::character::complete::{char, one_of};
use nom::combinator::{map, opt};
use nom::multi::{fold_many1, many1, separated_list0};
use nom::sequence::{delimited, pair, preceded, separated_pair, tuple};
use nom::IResult;

// TODO: Custom error handling

#[derive(Debug, Eq, PartialEq)]
pub struct Json {
    element: JsonElement,
}

#[derive(Debug, Eq, PartialEq)]
struct JsonElement {
    value: JsonValue,
}

impl JsonElement {
    fn new(value: JsonValue) -> JsonElement {
        JsonElement { value }
    }
}

// TODO: Use an actual map.
#[derive(Debug, Eq, PartialEq)]
struct Entry(String, JsonElement);

#[derive(Debug, Eq, PartialEq)]
enum JsonValue {
    JsonObject(Vec<Entry>),
    JsonArray(Vec<JsonElement>),
    JsonString(String),
    Number(Number),
    Boolean(bool),
    Null,
    // FIXME: Is this really necessary. Reconsider
    Empty,
}

pub fn parse(inp: &str) -> IResult<&str, Json> {
    println!("inside parse");
    map(parse_element, |elem| Json { element: elem })(inp)
}

fn parse_json_value(inp: &str) -> IResult<&str, JsonValue> {
    println!("inside parse json value");
    alt((
        parse_json_object,
        parse_json_array,
        map(parse_string, |s| JsonValue::JsonString(s)),
        parse_number,
        parse_null,
        parse_true,
        parse_false,
    ))(inp)
}

fn parse_json_object(inp: &str) -> IResult<&str, JsonValue> {
    println!("inside parse json object");
    delimited(
        char('{'),
        alt((map(parse_whitespace, |_| JsonValue::Empty), parse_members)),
        char('}'),
    )(inp)
}

fn parse_members(inp: &str) -> IResult<&str, JsonValue> {
    println!("inside parse json members: {:?}", inp);
    map(separated_list0(char(','), parse_member), |e| {
        JsonValue::JsonObject(e)
    })(inp)
}

fn parse_member(inp: &str) -> IResult<&str, Entry> {
    println!("inside parse member: {:?}", inp);
    map(
        separated_pair(
            delimited(opt(parse_whitespace), parse_string, opt(parse_whitespace)),
            char(':'),
            parse_element,
        ),
        |(s, v)| Entry(s, v),
    )(inp)
}

fn parse_json_array(inp: &str) -> IResult<&str, JsonValue> {
    delimited(
        char('['),
        alt((map(parse_whitespace, |_| JsonValue::Empty), parse_elements)),
        char(']'),
    )(inp)
}

fn parse_elements(inp: &str) -> IResult<&str, JsonValue> {
    map(separated_list0(char(','), parse_element), |l| {
        JsonValue::JsonArray(l)
    })(inp)
}

fn parse_element(inp: &str) -> IResult<&str, JsonElement> {
    println!("inside parse element");
    map(
        delimited(opt(parse_whitespace), parse_json_value, opt(parse_whitespace)),
        |jv| JsonElement::new(jv),
    )(inp)
}

fn parse_string(inp: &str) -> IResult<&str, String> {
    delimited(char('"'), parse_characters, char('"'))(inp)
}

// TODO: This is a shortcut. I just used the existing is_alphanumeric function here. Try to expand it
fn parse_characters(inp: &str) -> IResult<&str, String> {
    map(take_while(|c: char| c.is_alphanumeric()), |s| {
        String::from(s)
    })(inp)
}

fn parse_null(inp: &str) -> IResult<&str, JsonValue> {
    map(tag("null"), |_| JsonValue::Null)(inp)
}

fn parse_true(inp: &str) -> IResult<&str, JsonValue> {
    map(tag("true"), |_| JsonValue::Boolean(true))(inp)
}

fn parse_false(inp: &str) -> IResult<&str, JsonValue> {
    map(tag("false"), |_| JsonValue::Boolean(false))(inp)
}

#[derive(Debug, Eq, PartialEq)]
struct Hex(u32);

fn parse_hex(inp: &str) -> IResult<&str, Hex> {
    map(
        alt((
            parse_digit,
            map(one_of("abcdefABCDEF"), |v: char| v.to_digit(16).unwrap()),
        )),
        Hex,
    )(inp)
}

#[derive(Debug, Eq, PartialEq)]
struct Number(i32, Option<Fraction>, Option<Exponent>);

fn parse_number(inp: &str) -> IResult<&str, JsonValue> {
    map(
        tuple((parse_integer, opt(parse_fraction), opt(parse_exponent))),
        |(n, f, exp)| JsonValue::Number(Number(n, f, exp)),
    )(inp)
}

fn parse_integer(inp: &str) -> IResult<&str, i32> {
    let (inp, sign) = opt(char('-'))(inp)?;
    let sign_coef: i32 = match sign {
        Some('-') => -1,
        _ => 1,
    };
    map(
        alt((
            map(pair(parse_onenine, parse_digits), |(leading, rest)| {
                // FIXME: I am not a fan of all these string conversions. Find a better way
                let mut rest_str = rest.to_string();
                let leading_str = leading.to_string();
                rest_str.insert_str(0, &leading_str);
                rest_str.parse::<u32>().unwrap()
            }),
            parse_digit,
        )),
        move |v| (v as i32) * sign_coef,
    )(inp)
}

fn parse_digits(inp: &str) -> IResult<&str, u32> {
    fold_many1(parse_digit, || 0, |acc, val| acc * 10 + val)(inp)
}

// FIXME: Find a way to return char instead of u32. Currently `char(..)` function and
//  the type `char` have a name conflict.
fn parse_digit(inp: &str) -> IResult<&str, u32> {
    alt((map(char('0'), |c| c.to_digit(10).unwrap()), parse_onenine))(inp)
}

fn parse_onenine(inp: &str) -> IResult<&str, u32> {
    map(one_of("123456789"), |c| c.to_digit(10).unwrap())(inp)
}

// TODO: Change this representation. Storing this as f32 could make more sense
#[derive(Debug, Eq, PartialEq)]
struct Fraction(u32);

fn parse_fraction(inp: &str) -> IResult<&str, Fraction> {
    map(preceded(char('.'), parse_digits), Fraction)(inp)
}

#[derive(Debug, Eq, PartialEq)]
struct Exponent(Sign, u32);

// TODO: sign should be optional
fn parse_exponent(inp: &str) -> IResult<&str, Exponent> {
    map(
        preceded(alt((char('e'), char('E'))), pair(parse_sign, parse_digits)),
        |(sign, digits)| Exponent(sign, digits),
    )(inp)
}

#[derive(Debug, Eq, PartialEq)]
enum Sign {
    Plus,
    Minus,
}

fn parse_sign(inp: &str) -> IResult<&str, Sign> {
    alt((
        map(char('+'), |_| Sign::Plus),
        map(char('-'), |_| Sign::Minus),
    ))(inp)
}

#[derive(Debug, Eq, PartialEq)]
struct WhiteSpace;

fn parse_whitespace(inp: &str) -> IResult<&str, WhiteSpace> {
    map(
        many1(alt((
            char('\u{0020}'),
            char('\u{000A}'),
            char('\u{000D}'),
            char('\u{0009}'),
        ))),
        |_| WhiteSpace,
    )(inp)
}

#[cfg(test)]
mod test {
    use nom::error_position;
    use nom::Err;

    use super::*;

    #[test]
    fn test_parse_null() {
        assert_eq!(
            Ok(("nullnull", JsonValue::Null)),
            parse_null("nullnullnull")
        );
        assert_eq!(Ok(("", JsonValue::Null)), parse_null("null"));
        assert_eq!(
            Err(Err::Error(error_position!(
                "nonnull",
                nom::error::ErrorKind::Tag
            ))),
            parse_null("nonnull")
        );
    }

    #[test]
    fn test_parse_true() {
        assert_eq!(
            Ok(("true", JsonValue::Boolean(true))),
            parse_true("truetrue")
        );
        assert_eq!(Ok(("", JsonValue::Boolean(true))), parse_true("true"));
        assert_eq!(
            Err(Err::Error(error_position!(
                "false",
                nom::error::ErrorKind::Tag
            ))),
            parse_true("false")
        );
    }

    #[test]
    fn test_parse_false() {
        assert_eq!(
            Ok(("false", JsonValue::Boolean(false))),
            parse_false("falsefalse")
        );
        assert_eq!(Ok(("", JsonValue::Boolean(false))), parse_false("false"));
        assert_eq!(
            Err(Err::Error(error_position!(
                "true",
                nom::error::ErrorKind::Tag
            ))),
            parse_false("true")
        );
    }

    #[test]
    fn test_parse_hex() {
        assert_eq!(Ok(("", Hex(10))), parse_hex("a"));
        assert_eq!(Ok(("BC", Hex(10))), parse_hex("ABC"));
        assert_eq!(Ok(("6", Hex(2))), parse_hex("26"));
        assert_eq!(
            Err(Err::Error(error_position!(
                "KLM",
                nom::error::ErrorKind::OneOf
            ))),
            parse_hex("KLM")
        );
    }

    #[test]
    fn test_parse_number() {
        assert_eq!(
            Ok(("", JsonValue::Number(Number(56172, None, None)))),
            parse_number("56172")
        );
        assert_eq!(
            Ok(("", JsonValue::Number(Number(123, Some(Fraction(98)), None)))),
            parse_number("123.98")
        );
        assert_eq!(
            Ok((
                "",
                JsonValue::Number(Number(167, None, Some(Exponent(Sign::Minus, 12))))
            )),
            parse_number("167e-12")
        );
        assert_eq!(
            Ok((
                "",
                JsonValue::Number(Number(1, Some(Fraction(2)), Some(Exponent(Sign::Plus, 3))))
            )),
            parse_number("1.2e+3")
        );
        assert_eq!(
            Err(Err::Error(error_position!(
                "notanumber",
                nom::error::ErrorKind::OneOf
            ))),
            parse_number("notanumber")
        );
    }

    #[test]
    fn test_parse_integer() {
        assert_eq!(Ok(("", -341)), parse_integer("-341"));
        assert_eq!(Ok(("", 56398)), parse_integer("56398"));
        assert_eq!(Ok(("rem", 0)), parse_integer("0rem"));
        assert_eq!(
            Err(Err::Error(error_position!(
                "notaninteger",
                nom::error::ErrorKind::Many1
            ))),
            parse_digits("notaninteger")
        );
    }

    #[test]
    fn test_parse_digits() {
        assert_eq!(Ok(("wow", 345)), parse_digits("345wow"));
        assert_eq!(Ok(("", 2)), parse_digits("0002"));
        assert_eq!(
            Err(Err::Error(error_position!(
                "not digits",
                nom::error::ErrorKind::Many1
            ))),
            parse_digits("not digits")
        );
    }

    #[test]
    fn test_parse_digit() {
        assert_eq!(Ok(("45", 3)), parse_digit("345"));
        assert_eq!(Ok(("002", 0)), parse_digit("0002"));
        assert_eq!(
            Err(Err::Error(error_position!(
                "words",
                nom::error::ErrorKind::OneOf
            ))),
            parse_digit("words")
        );
    }

    #[test]
    fn test_parse_fraction() {
        assert_eq!(Ok(("", Fraction(24))), parse_fraction(".24"));
        assert_eq!(Ok(("rem", Fraction(1))), parse_fraction(".1rem"));
        assert_eq!(
            Err(Err::Error(error_position!(
                "not_frac",
                nom::error::ErrorKind::Char
            ))),
            parse_fraction("not_frac")
        );
        assert_eq!(
            Err(Err::Error(error_position!(
                "",
                nom::error::ErrorKind::Many1
            ))),
            parse_fraction(".")
        );
    }

    #[test]
    fn test_parse_exponent() {
        assert_eq!(Ok(("", Exponent(Sign::Plus, 36))), parse_exponent("E+36"));
        assert_eq!(
            Ok(("A", Exponent(Sign::Minus, 2222))),
            parse_exponent("e-2222A")
        );
        assert_eq!(
            Err(Err::Error(error_position!(
                "A+36",
                nom::error::ErrorKind::Char
            ))),
            parse_exponent("A+36")
        );
        assert_eq!(
            Err(Err::Error(error_position!(
                "*36",
                nom::error::ErrorKind::Char
            ))),
            parse_exponent("e*36")
        );
        assert_eq!(
            Err(Err::Error(error_position!(
                "notadigit",
                nom::error::ErrorKind::Many1
            ))),
            parse_exponent("e-notadigit")
        );
    }

    #[test]
    fn test_parse_sign() {
        assert_eq!(Ok(("12", Sign::Plus)), parse_sign("+12"));
        assert_eq!(Ok(("12", Sign::Minus)), parse_sign("-12"));
        assert_eq!(
            Err(Err::Error(error_position!(
                "12",
                nom::error::ErrorKind::Char
            ))),
            parse_sign("12")
        );
    }

    #[test]
    fn test_parse_whitespace() {
        assert_eq!(Ok(("rem", WhiteSpace)), parse_whitespace(" rem"));
        assert_eq!(Ok(("123", WhiteSpace)), parse_whitespace("\t\n   \r123"));
        assert_eq!(Ok(("", WhiteSpace)), parse_whitespace(" "));
        assert_eq!(
            Err(Err::Error(error_position!(
                "a",
                nom::error::ErrorKind::Char
            ))),
            parse_whitespace("a")
        );
    }
}
