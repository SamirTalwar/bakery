use nom::branch::alt;
use nom::bytes::complete::tag;
use nom::character::complete::{alpha1, line_ending, not_line_ending};
use nom::combinator::{all_consuming, complete, map, opt, recognize, value};
use nom::error::{make_error, ErrorKind};
use nom::multi::{many0, many1};
use nom::sequence::{delimited, pair, preceded, terminated};
use nom::Slice;
use nom_locate::{position, LocatedSpan};

use super::errors::{Error, Result};

type Span<'a> = LocatedSpan<&'a str>;

type ParseResult<'a, T> = nom::IResult<Span<'a>, T>;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Program {
    pub pipe: Pipe,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Pipe {
    pub source: Positioned<Expression>,
    pub sink: Positioned<Expression>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Expression {
    Identifier { namespace: String, id: String },
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Positioned<T> {
    pub position: Position,
    pub value: T,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Position {
    pub line: usize,
    pub column: usize,
    pub offset: usize,
}

pub fn parse(input: &str) -> Result<Program> {
    all_consuming(program)(Span::new(input))
        .map(|(_, result)| result)
        .map_err(|e| Error::ParseError(e.to_string()))
}

fn program(input: Span) -> ParseResult<Program> {
    map(delimited(markup, pipe, markup), |pipe| Program { pipe })(input)
}

fn pipe(input: Span) -> ParseResult<Pipe> {
    map(
        pair(expression, preceded(arrow, expression)),
        |(source, sink)| Pipe { source, sink },
    )(input)
}

fn arrow(input: Span) -> ParseResult<()> {
    value((), delimited(markup, tag("|>"), markup))(input)
}

fn expression(input: Span) -> ParseResult<Positioned<Expression>> {
    let (input, span) = position(input)?;
    let (input, value) = alt((namespaced_identifier, unnamespaced_identifier))(input)?;
    let position = Position {
        line: span.location_line() as usize,
        column: span.get_column(),
        offset: span.location_offset(),
    };
    Ok((input, Positioned { position, value }))
}

fn namespaced_identifier(input: Span) -> ParseResult<Expression> {
    let (input, namespace) = map(alpha1, fragment_string)(input)?;
    let (input, _) = tag(":")(input)?;
    let (input, id) = map(
        recognize(many1(complete(alphanumerisymbolic))),
        fragment_string,
    )(input)?;
    Ok((input, Expression::Identifier { namespace, id }))
}

fn unnamespaced_identifier(input: Span) -> ParseResult<Expression> {
    map(map(alpha1, fragment_string), |id| Expression::Identifier {
        namespace: String::from(""),
        id: id,
    })(input)
}

fn markup(input: Span) -> ParseResult<()> {
    value((), many0(alt((whitespace, complete(comment)))))(input)
}

fn comment(input: Span) -> ParseResult<&str> {
    preceded(tag("#"), line)(input)
}

// TODO: Support non-ASCII whitespace.
fn whitespace(input: Span) -> ParseResult<&str> {
    let length = (*input.fragment())
        .chars()
        .take_while(|character| character.is_whitespace())
        .collect::<String>()
        .len();
    if length > 0 {
        Ok((input.slice(length..), input.fragment().slice(..length)))
    } else {
        Err(nom::Err::Error(make_error(input, ErrorKind::Space)))
    }
}

fn line(input: Span) -> ParseResult<&str> {
    map(
        terminated(alt((not_line_ending, tag(""))), opt(line_ending)),
        fragment,
    )(input)
}

fn alphanumerisymbolic(input: Span) -> ParseResult<char> {
    match (*input.fragment()).chars().next() {
        Some(character) if !character.is_whitespace() && !character.is_control() => {
            Ok((input.slice(character.len_utf8()..), character))
        }
        _ => Err(nom::Err::Incomplete(nom::Needed::Size(1))),
    }
}

fn fragment(input: Span) -> &str {
    input.fragment()
}

fn fragment_string(input: Span) -> String {
    input.fragment().to_string()
}

#[cfg(test)]
mod tests {
    use super::*;

    use proptest;
    use proptest::collection;
    use proptest::prelude::*;

    proptest! {
        #[test]
        fn whitespace_matches_any_whitespace(input in "[ \\s]+") {
            let span = Span::new(&input);
            let parsed = whitespace(span)?;
            prop_assert_eq!((span.slice(input.len()..), input.as_str()), parsed);
        }

        #[test]
        fn whitespace_does_not_match_any_other_text(input in "[^ \\s]+.*") {
            let span = Span::new(&input);
            let parsed = whitespace(span);
            prop_assert!(parsed.is_err(), "Parsing succeeded: {:?}", parsed);
        }

        #[test]
        fn comment_matches_a_comment_marker_to_the_end_of_the_line(comment_text in "#\\PC*", next_line in any::<String>()) {
            let expected = comment_text[1..].to_string();
            let input = comment_text.to_string() + "\n" + &next_line;
            let span = Span::new(&input);
            let parsed = comment(span)?;
            prop_assert_eq!((span.slice(comment_text.len() + 1..), expected.as_str()), parsed);
        }

        #[test]
        fn comment_matches_a_comment_marker_on_the_last_line(input in "#\\PC*") {
            let expected = input[1..].to_string();
            let span = Span::new(&input);
            let parsed = comment(span)?;
            prop_assert_eq!((span.slice(input.len()..), expected.as_str()), parsed);
        }

        #[test]
        fn comment_does_not_match_anything_else(input in "|[^#]\\PC*") {
            let span = Span::new(&input);
            let parsed = comment(span);
            prop_assert!(parsed.is_err(), "Parsing succeeded: {:?}", parsed);
        }

        #[test]
        fn markup_matches_comments_and_whitespace(lines in collection::vec("[ \\s]*|#\\PC*", 0..5)) {
            let input = lines.join("\n");
            let span = Span::new(&input);
            let parsed = markup(span)?;
            prop_assert_eq!((span.slice(input.len()..), ()), parsed);
        }

        #[test]
        fn expression_matches_any_identifier(input in "[A-Za-z]+") {
            let span = Span::new(&input);
            let parsed = expression(span)?;
            let expected = Positioned {
                position: Position { line: 1, column: 1, offset: 0 },
                value: Expression::Identifier {
                    namespace: String::from(""),
                    id: input.clone(),
                },
            };
            prop_assert_eq!((span.slice(input.len()..), expected), parsed);
        }

        #[test]
        fn expression_matches_a_namespaced_identifier(namespace in "[A-Za-z]+", id in "[^\\pc\\s]+", rest in "\\s\\PC*") {
            let expression_string = namespace.clone() + ":" + &id;
            let input = expression_string.clone() + &rest;
            let span = Span::new(&input);
            let parsed = expression(span)?;
            let expected = Positioned {
                position: Position { line: 1, column: 1, offset: 0 },
                value: Expression::Identifier { namespace, id },
            };
            prop_assert_eq!((span.slice(expression_string.len()..), expected), parsed);
        }

        #[test]
        fn expression_does_not_match_anything_else(input in "[^A-Za-z].*") {
            let span = Span::new(&input);
            let parsed = expression(span);
            prop_assert!(parsed.is_err(), "Parsing succeeded: {:?}", parsed);
        }

        #[test]
        fn pipe_parses_a_pipe_from_source_to_sink(source_id in "[A-Za-z]+", sink_id in "[A-Za-z]+") {
            let input = source_id.clone() + " |> " + &sink_id;
            let span = Span::new(&input);
            let parsed = pipe(span)?;

            let sink_offset = source_id.len() + 4;
            let expected = Pipe {
                source: Positioned {
                    position: Position { line: 1, column: 1, offset: 0 },
                    value: Expression::Identifier {
                        namespace: String::from(""),
                        id: source_id,
                    },
                },
                sink: Positioned {
                    position: Position { line: 1, column: sink_offset + 1, offset: sink_offset },
                    value: Expression::Identifier {
                        namespace: String::from(""),
                        id: sink_id,
                    },
                },
            };
            prop_assert_eq!((span.slice(input.len()..), expected), parsed);
        }
    }
}
