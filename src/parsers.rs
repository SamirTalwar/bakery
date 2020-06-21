use nom::character::complete::{line_ending, not_line_ending};

named!(comment<&str, &str>,
       terminated!(preceded!(tag!("#"), not_line_ending), opt!(line_ending)));

#[cfg(test)]
mod tests {
    use super::*;
    use proptest;
    use proptest::prelude::*;

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
    }
}
