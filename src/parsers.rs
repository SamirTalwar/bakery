use nom::branch::alt;
use nom::bytes::complete::tag;
use nom::character::complete::{alpha1, line_ending, none_of, not_line_ending};
use nom::combinator::{all_consuming, complete, map, recognize, value};
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
    pub expression: Positioned<Expression>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Expression {
    Command {
        command: Token,
        arguments: Vec<Token>,
    },
    Pipe {
        source: Positioned<Expression>,
        sink: Positioned<Expression>,
    },
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Token {
    Raw { value: String },
    Identifier { namespace: String, id: String },
    Text { contents: String },
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Positioned<T> {
    pub position: Position,
    pub value: Box<T>,
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
        .map_err(|e| Error::Parse(e.to_string()))
}

fn program(input: Span) -> ParseResult<Program> {
    map(delimited(markup, expression, markup), |expression| {
        Program { expression }
    })(input)
}

fn arrow(input: Span) -> ParseResult<()> {
    value((), delimited(markup, tag("|>"), markup))(input)
}

fn positioned<'a, T, F>(parser: F) -> impl Fn(Span<'a>) -> ParseResult<Positioned<T>>
where
    F: Fn(Span<'a>) -> ParseResult<T> + 'a,
{
    move |input| {
        let (input, span) = position(input)?;
        let position = Position {
            line: span.location_line() as usize,
            column: span.get_column(),
            offset: span.location_offset(),
        };
        let (input, value) = parser(input)?;
        Ok((
            input,
            Positioned {
                position,
                value: Box::new(value),
            },
        ))
    }
}

fn expression(input: Span) -> ParseResult<Positioned<Expression>> {
    positioned(alt((pipe, command)))(input)
}

fn pipe(input: Span) -> ParseResult<Expression> {
    map(
        pair(positioned(command), preceded(arrow, expression)),
        |(source, sink)| Expression::Pipe { source, sink },
    )(input)
}

fn command(input: Span) -> ParseResult<Expression> {
    let (input, command) = token(input)?;
    let (input, arguments) = many0(token)(input)?;
    Ok((input, Expression::Command { command, arguments }))
}

fn token(input: Span) -> ParseResult<Token> {
    delimited(markup, alt((text, identifier, raw)), markup)(input)
}

fn text(input: Span) -> ParseResult<Token> {
    map(
        delimited(tag("\""), many0(none_of("\"")), tag("\"")),
        |contents| Token::Text {
            contents: contents.into_iter().collect(),
        },
    )(input)
}

fn identifier(input: Span) -> ParseResult<Token> {
    let (input, namespace) = map(alpha1, fragment_string)(input)?;
    let (input, _) = tag(":")(input)?;
    let (input, id) = map(
        recognize(many1(complete(alphanumerisymbolic))),
        fragment_string,
    )(input)?;
    Ok((input, Token::Identifier { namespace, id }))
}

fn raw(input: Span) -> ParseResult<Token> {
    map(map(alpha1, fragment_string), |value| Token::Raw { value })(input)
}

fn markup(input: Span) -> ParseResult<()> {
    value((), many0(alt((whitespace, comment))))(input)
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
        terminated(
            alt((not_line_ending, tag(""))),
            alt((value((), line_ending), eof)),
        ),
        fragment,
    )(input)
}

fn eof(input: Span) -> ParseResult<()> {
    use nom::InputLength;
    if (input).input_len() == 0 {
        Ok((input, ()))
    } else {
        Err(nom::Err::Error(make_error(input, ErrorKind::Eof)))
    }
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
    (*input.fragment()).to_string()
}

#[cfg(test)]
mod tests {
    use super::*;

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
        fn comment_matches_a_comment_marker_to_the_end_of_the_line(comment_text in "\\PC*", next_line in any::<String>()) {
            let input = "#".to_string() + &comment_text + "\n" + &next_line;
            let span = Span::new(&input);
            let parsed = comment(span)?;
            prop_assert_eq!((span.slice(comment_text.len() + 2..), comment_text.as_str()), parsed);
        }

        #[test]
        fn comment_matches_a_comment_marker_on_the_last_line(comment_text in "\\PC*") {
            let input = "#".to_string() + &comment_text;
            let span = Span::new(&input);
            let parsed = comment(span)?;
            prop_assert_eq!((span.slice(input.len()..), comment_text.as_str()), parsed);
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
        fn token_matches_any_identifier(input in "[A-Za-z]+") {
            let span = Span::new(&input);
            let parsed = token(span)?;

            let expected = Token::Raw { value: input.clone() };
            prop_assert_eq!((span.slice(input.len()..), expected), parsed);
        }

        #[test]
        fn token_matches_a_namespaced_identifier(namespace in "[A-Za-z]+", id in "[^\\pc\\s]+") {
            let input = namespace.clone() + ":" + &id;
            let span = Span::new(&input);
            let parsed = token(span)?;

            let expected = Token::Identifier { namespace, id };
            prop_assert_eq!((span.slice(input.len()..), expected), parsed);
        }

        #[test]
        fn token_matches_a_text_string(text in "[^\"]*") {
            let input = "\"".to_string() + &text + "\"";
            let span = Span::new(&input);
            let parsed = token(span)?;

            let expected = Token::Text { contents: text };
            prop_assert_eq!((span.slice(input.len()..), expected), parsed);
        }

        #[test]
        fn expression_parses_a_pipe_from_source_to_sink(source_id in "[A-Za-z]+", sink_id in "[A-Za-z]+") {
            let input = source_id.clone() + " |> " + &sink_id;
            let span = Span::new(&input);
            let parsed = expression(span)?;

            let sink_offset = source_id.len() + 4;
            let expected = Positioned {
                position: Position { line: 1, column: 1, offset: 0 },
                value: Box::new(Expression::Pipe {
                    source: Positioned {
                        position: Position { line: 1, column: 1, offset: 0 },
                        value: Box::new(Expression::Command {
                            command: Token::Raw { value: source_id },
                            arguments: vec![],
                        }),
                    },
                    sink: Positioned {
                        position: Position { line: 1, column: sink_offset + 1, offset: sink_offset },
                        value: Box::new(Expression::Command {
                            command: Token::Raw { value: sink_id },
                            arguments: vec![],
                        }),
                    },
                }),
            };
            prop_assert_eq!((span.slice(input.len()..), expected), parsed);
        }

        #[test]
        fn expression_parses_two_pipes(
            source_id in "[A-Za-z]+",
            conduit_id in "[A-Za-z]+",
            sink_id in "[A-Za-z]+",
        ) {
            let input = source_id.clone() + " |> " + &conduit_id + " |> " + &sink_id;
            let span = Span::new(&input);
            let parsed = expression(span)?;

            let conduit_offset = source_id.len() + 4;
            let sink_offset = conduit_offset + conduit_id.len() + 4;
            let expected = Positioned {
                position: Position { line: 1, column: 1, offset: 0 },
                value: Box::new(Expression::Pipe {
                    source: Positioned {
                        position: Position { line: 1, column: 1, offset: 0 },
                        value: Box::new(Expression::Command {
                            command: Token::Raw { value: source_id },
                            arguments: vec![],
                        }),
                    },
                    sink:  Positioned {
                        position: Position { line: 1, column: conduit_offset + 1, offset: conduit_offset },
                        value: Box::new(Expression::Pipe {
                            source: Positioned {
                                position: Position { line: 1, column: conduit_offset + 1, offset: conduit_offset },
                                value: Box::new(Expression::Command {
                                    command: Token::Raw { value: conduit_id },
                                    arguments: vec![],
                                })
                            },
                            sink: Positioned {
                                position: Position { line: 1, column: sink_offset + 1, offset: sink_offset },
                                value: Box::new(Expression::Command {
                                    command: Token::Raw { value: sink_id },
                                    arguments: vec![],
                                })
                            },
                        }),
                    },
                }),
            };
            prop_assert_eq!((span.slice(input.len()..), expected), parsed);
        }

        #[test]
        fn expression_matches_a_command_with_no_arguments(id in "[A-Za-z]+") {
            let offset = id.len();
            let span = Span::new(&id);
            let parsed = expression(span)?;

            let expected = Positioned {
                position: Position { line: 1, column: 1, offset: 0 },
                value: Box::new(Expression::Command {
                    command: Token::Raw { value: id.clone() },
                    arguments: vec![],
                })
            };
            prop_assert_eq!((span.slice(offset..), expected), parsed);
        }

        #[test]
        fn expression_matches_a_command_with_some_arguments(
            arguments in collection::vec("[A-Za-z]+", 1..5),
            spaces in collection::vec("\\s+", 5),
        ) {
            let input = arguments
                .iter()
                .zip(spaces.iter())
                .flat_map(|(arg, space)| vec![arg, space])
                .cloned()
                .collect::<String>();
            let span = Span::new(&input);
            let parsed = expression(span)?;

            let command = Token::Raw { value: arguments[0].clone() };
            let arguments = arguments
                .iter()
                .skip(1)
                .map(|id| Token::Raw { value: id.to_string() })
                .collect();
            let expected = Positioned {
                position: Position { line: 1, column: 1, offset: 0 },
                value: Box::new(Expression::Command { command, arguments })
            };
            prop_assert_eq!((span.slice(input.len()..), expected), parsed);
        }

        #[test]
        fn expression_does_not_match_anything_else(input in "\\s*[^\\sA-Za-z\"].*") {
            let span = Span::new(&input);
            let parsed = expression(span);
            prop_assert!(parsed.is_err(), "Parsing succeeded: {:?}", parsed);
        }

        #[test]
        fn expression_strips_markup(
            prefix in "\\s*(#[^\\PC\\r\\n]+\\n)?",
            command in "[A-Za-z]+",
            suffix in "\\s*(#[^\\PC\\r\\n]+)?",
        ) {
            let input = prefix + &command + &suffix;
            let span = Span::new(&input);
            let parsed = expression(span)?;

            let expected = Positioned {
                position: Position { line: 1, column: 1, offset: 0 },
                value: Box::new(Expression::Command {
                    command: Token::Raw { value: command },
                    arguments: vec![],
                })
            };
            prop_assert_eq!((span.slice(input.len()..), expected), parsed);
        }
    }
}
