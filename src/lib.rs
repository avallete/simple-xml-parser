#[derive(Clone, Debug, PartialEq, Eq)]
struct Element {
    name: String,
    attributes: Vec<(String, String)>,
    children: Vec<Element>,
}

fn match_literal(expected: &'static str) -> impl Fn(&str) -> Result<(&str, ()), &str> {
    move |input| match input.get(0..expected.len()) {
        Some(next) if next == expected => Ok((&input[expected.len()..], ())),
        _ => Err(input),
    }
}

#[cfg(test)]
mod tests {
    #[test]
    fn it_works() {
        assert_eq!(2 + 2, 4);
    }

    #[cfg(test)]
    mod match_literal_tests {
        use crate::match_literal;

        #[test]
        fn should_return_end_of_sentence() {
            let parse_joe = match_literal("Hello Joe!");
            assert_eq!(Ok(("", ())), parse_joe("Hello Joe!"));
        }

        #[test]
        fn should_return_position_after_result() {
            let parse_joe = match_literal("Hello Joe!");
            assert_eq!(
                Ok((" Hello Robert !", ())),
                parse_joe("Hello Joe! Hello Robert !")
            );
        }

        #[test]
        fn should_return_error_with_input_if_not_found() {
            let parse_joe = match_literal("Hello Joe!");
            assert_eq!(Err("Hello Mike!"), parse_joe("Hello Mike!"));
        }
    }
}
