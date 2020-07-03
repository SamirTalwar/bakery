use std::io;
use std::ops;

use nom::bytes::complete::tag;
use nom::character::complete::{alpha1, line_ending, multispace1, not_line_ending, space0};
use nom::sequence::delimited;
use nom::IResult;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Identifier(pub String);

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Expression {
    pub identifier: Identifier,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Pipe {
    pub source: Expression,
    pub sink: Expression,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Program {
    pub pipe: Pipe,
}

fn spaced<'a, T>(
    parser: impl ops::Fn(&'a str) -> IResult<&'a str, T>,
) -> impl ops::Fn(&'a str) -> IResult<&'a str, T> {
    delimited(space0, parser, space0)
}

named!(line<&str, &str>, terminated!(alt!(not_line_ending | tag!("")), opt!(line_ending)));

named!(comment<&str, &str>, preceded!(tag!("#"), line));

// TODO: Support non-ASCII whitespace.
named!(whitespace<&str, &str>, recognize!(multispace1));

named!(markup<&str, ()>, alt!(
    value!((), many0!(alt!(whitespace | complete!(comment))))
));

named!(identifier<&str, Identifier>, map!(alpha1, |id| Identifier(id.to_string())));

named!(expression<&str, Expression>, map!(identifier, |identifier| Expression { identifier }));

named!(pipe<&str, Pipe>,
       map!(pair!(expression, preceded!(arrow, expression)),
            |(source, sink)| Pipe { source, sink }));

fn arrow(input: &str) -> IResult<&str, ()> {
    let (input, _) = spaced(tag("|>"))(input)?;
    Ok((input, ()))
}

named!(program<&str, Program>, exact!(map!(delimited!(markup, pipe, markup), |pipe| Program { pipe })));

pub fn parse(input: &str) -> io::Result<Program> {
    program(input)
        .map(|(_, result)| result)
        .map_err(|e| io::Error::new(io::ErrorKind::Other, e.to_string()))
}

#[cfg(test)]
mod tests {
    use super::*;
    use proptest;
    use proptest::collection;
    use proptest::prelude::*;

    prop_compose! {
        fn any_identifier()(id in "[A-Za-z]+") -> Identifier {
            Identifier(id)
        }
    }

    proptest! {
        #[test]
        fn whitespace_matches_any_whitespace(input in "[ \t\r\n]+") {
            let parsed = whitespace(&input)?;
            prop_assert_eq!(("", input.as_str()), parsed);
        }

        #[test]
        fn whitespace_does_not_match_any_other_text(input in "[^ \t\r\n]+.*") {
            let parsed = whitespace(&input);
            prop_assert!(parsed.is_err(), "Parsing succeeded: {:?}", parsed);
        }

        #[test]
        fn comment_matches_a_comment_marker_to_the_end_of_the_line(comment_text in "#\\PC*", next_line in any::<String>()) {
            let expected = comment_text[1..].to_string();
            let input = comment_text.to_string() + "\n" + &next_line;
            let parsed = comment(&input)?;
            prop_assert_eq!((next_line.as_str(), expected.as_str()), parsed);
        }

        #[test]
        fn comment_matches_a_comment_marker_on_the_last_line(input in "#\\PC*") {
            let expected = input[1..].to_string();
            let parsed = comment(&input)?;
            prop_assert_eq!(("", expected.as_str()), parsed);
        }

        #[test]
        fn comment_does_not_match_anything_else(input in "|[^#]\\PC*") {
            let parsed = comment(&input);
            prop_assert!(parsed.is_err(), "Parsing succeeded: {:?}", parsed);
        }

        #[test]
        fn markup_matches_comments_and_whitespace(lines in collection::vec("[ \t\r\n]*|#\\PC*", 0..5)) {
            let input = lines.join("\n");
            let parsed = markup(&input)?;
            prop_assert_eq!(("", ()), parsed);
        }

        #[test]
        fn identifier_matches_any_identifier(input in "[A-Za-z]+") {
            let parsed = identifier(&input)?;
            prop_assert_eq!(("", Identifier(input.clone())), parsed);
        }

        #[test]
        fn identifier_does_not_match_anything_else(input in "[^A-Za-z].*") {
            let parsed = identifier(&input);
            prop_assert!(parsed.is_err(), "Parsing succeeded: {:?}", parsed);
        }

        #[test]
        fn pipe_parses_a_pipe_from_source_to_sink(source_id in any_identifier(), sink_id in any_identifier()) {
            let input = source_id.0.clone() + " |> " + &sink_id.0;
            let parsed = pipe(&input)?;

            let expected = Pipe {
                source: Expression { identifier: source_id },
                sink: Expression { identifier: sink_id },
            };
            prop_assert_eq!(("", expected), parsed);
        }
    }
}
