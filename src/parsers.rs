use nom::branch::alt;
use nom::bytes::complete::tag;
use nom::character::complete::{alpha1, line_ending, multispace1, not_line_ending};
use nom::combinator::{all_consuming, complete, map, opt, value};
use nom::multi::many0;
use nom::sequence::{delimited, pair, preceded, terminated};
use nom_locate::{position, LocatedSpan};

use super::errors::{Error, Result};

type Span<'a> = LocatedSpan<&'a str>;

type ParseResult<'a, T> = nom::IResult<Span<'a>, T>;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Program<'a> {
    pub pipe: Pipe<'a>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Pipe<'a> {
    pub source: Expression<'a>,
    pub sink: Expression<'a>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Expression<'a> {
    pub identifier: Identifier<'a>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Identifier<'a> {
    value: String,
    position: Span<'a>,
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

fn expression(input: Span) -> ParseResult<Expression> {
    map(identifier, |identifier| Expression { identifier })(input)
}

fn identifier(input: Span) -> ParseResult<Identifier> {
    let (input, position) = position(input)?;
    let (input, value) = map(alpha1, |result: Span| result.fragment().to_string())(input)?;
    Ok((input, Identifier { value, position }))
}

fn markup(input: Span) -> ParseResult<()> {
    value((), many0(alt((whitespace, complete(comment)))))(input)
}

fn comment(input: Span) -> ParseResult<&str> {
    preceded(tag("#"), line)(input)
}

// TODO: Support non-ASCII whitespace.
fn whitespace(input: Span) -> ParseResult<&str> {
    map(multispace1, |result: Span| *result.fragment())(input)
}

fn line(input: Span) -> ParseResult<&str> {
    map(
        terminated(alt((not_line_ending, tag(""))), opt(line_ending)),
        |result: Span| *result.fragment(),
    )(input)
}

#[cfg(test)]
mod tests {
    use super::*;

    use nom::Slice;

    use proptest;
    use proptest::collection;
    use proptest::prelude::*;

    proptest! {
        #[test]
        fn whitespace_matches_any_whitespace(input in "[ \t\r\n]+") {
            let span = Span::new(&input);
            let parsed = whitespace(span)?;
            prop_assert_eq!((span.slice(input.len()..), input.as_str()), parsed);
        }

        #[test]
        fn whitespace_does_not_match_any_other_text(input in "[^ \t\r\n]+.*") {
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
        fn markup_matches_comments_and_whitespace(lines in collection::vec("[ \t\r\n]*|#\\PC*", 0..5)) {
            let input = lines.join("\n");
            let span = Span::new(&input);
            let parsed = markup(span)?;
            prop_assert_eq!((span.slice(input.len()..), ()), parsed);
        }

        #[test]
        fn identifier_matches_any_identifier(input in "[A-Za-z]+") {
            let span = Span::new(&input);
            let expected = Identifier { value: input.clone(), position: span.slice(0..0) };
            let parsed = identifier(span)?;
            prop_assert_eq!((span.slice(input.len()..), expected), parsed);
        }

        #[test]
        fn identifier_does_not_match_anything_else(input in "[^A-Za-z].*") {
            let span = Span::new(&input);
            let parsed = identifier(span);
            prop_assert!(parsed.is_err(), "Parsing succeeded: {:?}", parsed);
        }

        #[test]
        fn pipe_parses_a_pipe_from_source_to_sink(source_id in "[A-Za-z]+", sink_id in "[A-Za-z]+") {
            let input = source_id.clone() + " |> " + &sink_id;
            let span = Span::new(&input);
            let parsed = pipe(span)?;

            let sink_offset = source_id.len() + 4;
            let expected = Pipe {
                source: Expression {
                    identifier: Identifier {
                        value: source_id.clone(),
                        position: span.slice(0..0),
                    },
                },
                sink: Expression {
                    identifier: Identifier {
                        value: sink_id.clone(),
                        position: span.slice(sink_offset..sink_offset),
                    },
                },
            };
            prop_assert_eq!((span.slice(input.len()..), expected), parsed);
        }
    }
}
