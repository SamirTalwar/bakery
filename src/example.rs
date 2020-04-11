fn add(a: i64, b: i64) -> i64 {
    i64::overflowing_add(a, b).0
}

#[cfg(test)]
mod tests {
    use super::*;
    use proptest;
    use proptest::prelude::*;

    #[test]
    fn adds_small_numbers() {
        assert_eq!(5, add(2, 3));
    }

    proptest! {
        #[test]
        fn adds(a in proptest::num::i64::ANY, b in proptest::num::i64::ANY) {
            prop_assert_eq!(i64::overflowing_add(a, b).0, add(a, b));
        }
    }
}
