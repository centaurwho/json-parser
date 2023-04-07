use nom::{Finish, IResult};
use nom::branch::alt;
use nom::bytes::complete::tag;
use nom::character::complete::{char, digit0, digit1};
use nom::combinator::map;
use nom::multi::{fold_many0, fold_many1, many0, separated_list0};
use nom::sequence::{delimited, pair, preceded, separated_pair};

struct Key;

struct Value;

#[allow(dead_code)]
struct Json {
    element: JsonElement,
}

impl Json {
    fn parse(inp: &str) -> Json {
        let element = JsonElement::parse(inp).finish().unwrap().1;
        Json { element }
    }
}

#[derive(Debug, Eq, PartialEq)]
struct JsonElement {
    value: JsonValue,
}

impl JsonElement {
    fn parse(inp: &str) -> IResult<&str, JsonElement> {
        unimplemented!()
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
    // TODO: Find a way to discard this cleanly
    WhiteSpace(char),
}

#[derive(Debug, Eq, PartialEq)]
enum Number {
    Integer(i64)
}

#[allow(dead_code)]
fn parse_json_value(inp: &str) -> IResult<&str, JsonValue> {
    alt((
        parse_json_object,
        parse_json_array,
        parse_string,
        parse_number,
        parse_null,
        parse_true,
        parse_false
    ))(inp)
}

fn parse_json_array(inp: &str) -> IResult<&str, JsonValue> {
    unimplemented!()
}

fn parse_string(inp: &str) -> IResult<&str, JsonValue> {
    unimplemented!()
}

fn parse_number(inp: &str) -> IResult<&str, JsonValue> {
    unimplemented!()
}

fn parse_json_object(inp: &str) -> IResult<&str, JsonValue> {
    // delimited(char('{'), alt((parse_whitespace, parse_members)), char('}'))(inp)
    unimplemented!()
}

fn parse_members(inp: &str) -> IResult<&str, Vec<Entry>> {
    unimplemented!()
    // separated_list0(',', parse_member)(inp)
}

fn parse_member(inp: &str) -> IResult<&str, Entry> {
    // map(separated_pair(parse_key, char(','), parse_element), |(val1, val2)| Entry(val1, val2))(inp)
    unimplemented!()
}

fn parse_key(inp: &str) -> IResult<&str, JsonValue> {
    unimplemented!()
}

fn parse_element(inp: &str) -> IResult<&str, JsonValue> {
    unimplemented!()
}

fn parse_null(inp: &str) -> IResult<&str, JsonValue> {
    map(tag("null"), |s: &str| JsonValue::Null)(inp)
}

fn parse_true(inp: &str) -> IResult<&str, JsonValue> {
    map(tag("true"), |s: &str| JsonValue::Boolean(true))(inp)
}

fn parse_false(inp: &str) -> IResult<&str, JsonValue> {
    map(tag("false"), |s: &str| JsonValue::Boolean(false))(inp)
}

fn parse_value(inp: &str) -> IResult<&str, JsonValue> {
    delimited(char('{'), parse_json_value, char('}'))(inp)
}

fn parse_digits(inp: &str) -> IResult<&str, u32> {
    fold_many1(
        parse_digit,
        || {0},
        |acc, val| acc * 10 + val
    )(inp)
}

// TODO: Find a way to simplify
fn parse_digit(inp: &str) -> IResult<&str, u32> {
    map(alt((
        char('0'),
        char('1'),
        char('2'),
        char('3'),
        char('4'),
        char('5'),
        char('6'),
        char('7'),
        char('8'),
        char('9')
    )), |c: char| c.to_digit(10).unwrap())(inp)
}

struct Exponent(Sign, u32);

// TODO: sign should be optional
fn parse_exponent(inp: &str) -> IResult<&str, Exponent> {
    map(preceded(
        alt((char('e'), char('E'))),
        pair(parse_sign, parse_digits),
    ), |(sign, digits)| Exponent(sign, digits))(inp)
}

#[derive(Debug, Eq, PartialEq)]
enum Sign {
    PLUS,
    MINUS,
}

fn parse_sign(inp: &str) -> IResult<&str, Sign> {
    alt((
        map(char('+'), |_| Sign::PLUS),
        map(char('-'), |_| Sign::MINUS))
    )(inp)
}

fn parse_whitespace(inp: &str) -> IResult<&str, JsonValue> {
    map(alt((
        char('\u{0020}'),
        char('\u{000A}'),
        char('\u{000D}'),
        char('\u{0009}'))
    ), |c| JsonValue::WhiteSpace(c),
    )(inp)
}

#[cfg(test)]
mod test {
    use nom::CompareResult::Error;
    use nom::Err;
    use nom::error_position;

    use super::*;

    #[test]
    fn test_parse_null() {
        assert_eq!(Ok(("nullnull", JsonValue::Null)), parse_null("nullnullnull"));
        assert_eq!(Ok(("", JsonValue::Null)), parse_null("null"));
        assert_eq!(Err(Err::Error(error_position!("nonnull", nom::error::ErrorKind::Tag))), parse_null("nonnull"));
    }

    #[test]
    fn test_parse_true() {
        assert_eq!(Ok(("true", JsonValue::Boolean(true))), parse_true("truetrue"));
        assert_eq!(Ok(("", JsonValue::Boolean(true))), parse_true("true"));
        assert_eq!(Err(Err::Error(error_position!("false", nom::error::ErrorKind::Tag))), parse_true("false"));
    }

    #[test]
    fn test_parse_false() {
        assert_eq!(Ok(("false", JsonValue::Boolean(false))), parse_false("falsefalse"));
        assert_eq!(Ok(("", JsonValue::Boolean(false))), parse_false("false"));
        assert_eq!(Err(Err::Error(error_position!("true", nom::error::ErrorKind::Tag))), parse_false("true"));
    }

    #[test]
    fn test_parse_digits() {
        assert_eq!(Ok(("wow", 345)), parse_digits("345wow"));
        assert_eq!(Ok(("", 2)), parse_digits("0002"));
        assert_eq!(Err(Err::Error(error_position!("not digits", nom::error::ErrorKind::Many1))), parse_digits("not digits"));
    }

    #[test]
    fn test_parse_digit() {
        assert_eq!(Ok(("45", 3)), parse_digit("345"));
        assert_eq!(Ok(("002", 0)), parse_digit("0002"));
        assert_eq!(Err(Err::Error(error_position!("words", nom::error::ErrorKind::Char))), parse_digit("words"));
    }

    #[test]
    fn test_parse_sign() {
        assert_eq!(Ok(("12", Sign::PLUS)), parse_sign("+12"));
        assert_eq!(Ok(("12", Sign::MINUS)), parse_sign("-12"));
        assert_eq!(Err(Err::Error(error_position!("12", nom::error::ErrorKind::Char))), parse_sign("12"));
    }

    #[test]
    fn test_parse_whitespace() {
        assert_eq!(Ok(("", JsonValue::WhiteSpace(' '))), parse_whitespace(" "));
        assert_eq!(Ok(("", JsonValue::WhiteSpace('\t'))), parse_whitespace("\t"));
        assert_eq!(Ok(("", JsonValue::WhiteSpace('\n'))), parse_whitespace("\n"));
        assert_eq!(Ok(("", JsonValue::WhiteSpace('\r'))), parse_whitespace("\r"));
        assert_eq!(Err(Err::Error(error_position!("a", nom::error::ErrorKind::Char))), parse_whitespace("a"));
    }
}
