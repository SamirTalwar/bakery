use std::error;
use std::fmt;

pub type Input<'a> = &'a str;

pub type Parsed<'a, T> = Result<(Input<'a>, T), Error<'a>>;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Error<'a> {
    Rejected(&'a str),
}

impl<'a> fmt::Display for Error<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> Result<(), fmt::Error> {
        match self {
            Self::Rejected(remainder) => write!(f, "Rejected: {}", remainder),
        }
    }
}

impl<'a> error::Error for Error<'a> {}

pub fn match_literal<'literal: 'a, 'a>(
    expected: &'literal str,
) -> impl Fn(Input<'a>) -> Parsed<'_, ()> {
    move |input| match input.get(0..expected.len()) {
        Some(next) if next == expected => Ok((&input[expected.len()..], ())),
        _ => Err(Error::Rejected(input)),
    }
}

pub fn comment<'a>(input: Input<'a>) -> Parsed<'_, ()> {
    let (text, _) = match_literal("#")(input)?;
    match text.find("\n") {
        Some(newline) => Ok((&text[newline + 1..], ())),
        None => Ok(("", ())),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use proptest;
    use proptest::prelude::*;

    proptest! {
        #[test]
        fn match_literal_consumes(expected in any::<String>()) {
            let string = expected.clone();
            let parsed = match_literal(&expected)(&string)?;
            prop_assert_eq!(("", ()), parsed);
        }

        #[test]
        fn match_literal_can_be_chained(expected_a in any::<String>(), expected_b in any::<String>(), expected_c in any::<String>()) {
            let string = expected_a.clone() + &expected_b + &expected_c;

            let parsed_a = match_literal(&expected_a)(&string)?;
            let expected_remainder_a: String = expected_b.clone() + &expected_c;
            prop_assert_eq!((expected_remainder_a.as_str(), ()), parsed_a);

            let parsed_b = match_literal(&expected_b)(parsed_a.0)?;
            prop_assert_eq!((expected_c.as_str(), ()), parsed_b);

            let parsed_c = match_literal(&expected_c)(parsed_b.0)?;
            prop_assert_eq!(("", ()), parsed_c);
        }

        #[test]
        fn match_literal_rejects_any_other_input(expected in any::<String>(), input in any::<String>()) {
            prop_assume!(!input.starts_with(&expected));

            let result = match_literal(&expected)(&input);
            prop_assert_eq!(Err(Error::Rejected(&input)), result);
        }

        #[test]
        fn comment_matches_a_comment_marker_to_the_end_of_the_line(comment_text in any::<String>()) {
            let input = "#".to_string() + &comment_text + "\n";
            let parsed = comment(&input)?;
            prop_assert_eq!(("", ()), parsed);
        }

        #[test]
        fn comment_matches_a_comment_marker_on_the_last_line(comment_text in any::<String>()) {
            let input = "#".to_string() + &comment_text;
            let parsed = comment(&input)?;
            prop_assert_eq!(("", ()), parsed);
        }

        #[test]
        fn comment_does_not_match_anything_else(input in "[^#]\\PC*") {
            let parsed = comment(&input);
            prop_assert_eq!(Err(Error::Rejected(&input)), parsed);
        }
    }
}
