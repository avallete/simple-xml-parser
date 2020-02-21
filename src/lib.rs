#[derive(Clone, Debug, PartialEq, Eq)]
struct Element {
    name: String,
    attributes: Vec<(String, String)>,
    children: Vec<Element>,
}

fn match_literal(expected: &'static str) -> impl Fn(&str) -> Result<(&str, ()), &str> {
    move |input| match input.find(expected) {
        Some(pos) => Ok((&input[pos + expected.len()..], ())),
        _ => Err(input),
    }
}

fn identifier(input: &str) -> Result<(&str, String), &str> {
    let mut matched = String::new();
    let mut chars = input.chars();

    match chars.next() {
        Some(next) if next.is_alphabetic() => matched.push(next),
        _ => return Err(input),
    }

    while let Some(next) = chars.next() {
        if next.is_alphanumeric() || next == '-' {
            matched.push(next);
        } else {
            break;
        }
    }

    Ok((&input[matched.len()..], matched))
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
            assert_eq!(parse_joe("Hello Joe!"), Ok(("", ())));
        }

        #[test]
        fn should_return_position_after_result() {
            let parse_joe = match_literal("Hello Joe!");
            assert_eq!(
                parse_joe("Hello Joe! Hello Robert !"),
                Ok((" Hello Robert !", ())),
            );
        }

        #[test]
        fn should_return_error_with_input_if_not_found() {
            let parse_joe = match_literal("Hello Joe!");
            assert_eq!(parse_joe("Hello Mike!"), Err("Hello Mike!"),);
        }
    }

    #[cfg(test)]
    mod identifier_tests {
        use crate::identifier;

        #[test]
        fn alphabetic_identifier() {
            assert_eq!(
                identifier("iamanidentifier"),
                Ok(("", "iamanidentifier".to_string())),
            );
        }

        #[test]
        fn alphanumeric_identifier() {
            assert_eq!(
                identifier("i4m4nidentifier"),
                Ok(("", "i4m4nidentifier".to_string())),
            );
        }

        #[test]
        fn identifier_with_dash() {
            assert_eq!(
                identifier("i-am-an-identifier"),
                Ok(("", "i-am-an-identifier".to_string())),
            );
        }

        #[test]
        fn identifier_in_sentence() {
            assert_eq!(
                identifier("iam an identifier"),
                Ok((" an identifier", "iam".to_string())),
            );
        }

        #[test]
        fn one_char_identifier() {
            assert_eq!(
                identifier("i am identifier"),
                Ok((" am identifier", "i".to_string())),
            );
        }

        #[test]
        fn invalid_identifier_starting_with_digit() {
            assert_eq!(identifier("1am an identifier"), Err("1am an identifier"),);
        }

        #[test]
        fn invalid_identifier() {
            assert_eq!(identifier("!iamidentifier"), Err("!iamidentifier"),);
        }
    }
}
