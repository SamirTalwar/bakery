use super::types::*;

use std::ops;

use nom::bytes::complete::tag;
use nom::character::complete::{alpha1, line_ending, not_line_ending, space0};
use nom::sequence::delimited;
use nom::IResult;

fn spaced<'a, T>(
    parser: impl ops::Fn(&'a str) -> IResult<&'a str, T>,
) -> impl ops::Fn(&'a str) -> IResult<&'a str, T> {
    delimited(space0, parser, space0)
}

named!(comment<&str, &str>,
       terminated!(preceded!(tag!("#"), not_line_ending), opt!(line_ending)));

named!(identifier<&str, Identifier>, map!(alpha1, |id| Identifier(id.to_string())));

named!(expression<&str, Expression>, map!(identifier, |identifier| Expression { identifier }));

named!(pipe<&str, Pipe>,
       map!(pair!(expression, preceded!(arrow, expression)),
            |(source, sink)| Pipe { source, sink }));

fn arrow(input: &str) -> IResult<&str, ()> {
    let (input, _) = spaced(tag("|>"))(input)?;
    Ok((input, ()))
}

#[cfg(test)]
mod tests {
    use super::*;
    use proptest;
    use proptest::prelude::*;

    prop_compose! {
        fn any_identifier()(id in "[A-Za-z]+") -> Identifier {
            Identifier(id)
        }
    }

    proptest! {
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
        fn comment_does_not_match_anything_else(input in "[^#]\\PC*") {
            let parsed = comment(&input);
            prop_assert!(parsed.is_err());
        }

        #[test]
        fn identifier_matches_any_identifier(input in "[A-Za-z]+") {
            let parsed = identifier(&input)?;
            prop_assert_eq!(("", Identifier(input.clone())), parsed);
        }

        #[test]
        fn identifier_does_not_match_anything_else(input in "[^A-Za-z].*") {
            let parsed = identifier(&input);
            prop_assert!(parsed.is_err());
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
