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

pub fn match_literal<'a>(expected: &'a str) -> impl Fn(Input<'a>) -> Parsed<'a, ()> {
    move |input| match input.get(0..expected.len()) {
        Some(next) if next == expected => Ok((&input[expected.len()..], ())),
        _ => Err(Error::Rejected(input)),
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
    }
}
