use std::ops::{Bound, Range, RangeBounds, RangeInclusive};

#[derive(Clone, Debug, PartialEq, Eq)]
struct Element {
    name: String,
    attributes: Vec<(String, String)>,
    children: Vec<Element>,
}

type ParseResult<'a, Output> = Result<(&'a str, Output), &'a str>;

trait Parser<'a, Output> {
    fn parse(&self, input: &'a str) -> ParseResult<'a, Output>;
}

impl<'a, F, Output> Parser<'a, Output> for F
where
    F: Fn(&'a str) -> ParseResult<Output>,
{
    fn parse(&self, input: &'a str) -> ParseResult<'a, Output> {
        self(input)
    }
}

fn map<'a, P, F, A, B>(parser: P, map_fn: F) -> impl Parser<'a, B>
where
    P: Parser<'a, A>,
    F: Fn(A) -> B,
{
    move |input| {
        parser
            .parse(input)
            .map(|(next_input, result)| (next_input, map_fn(result)))
    }
}

fn pair<'a, P1, P2, R1, R2>(parser1: P1, parser2: P2) -> impl Parser<'a, (R1, R2)>
where
    P1: Parser<'a, R1>,
    P2: Parser<'a, R2>,
{
    move |input| {
        parser1.parse(input).and_then(|(next_input, result1)| {
            parser2
                .parse(next_input)
                .map(|(last_input, result2)| (last_input, (result1, result2)))
        })
    }
}

fn left<'a, P1, P2, R1, R2>(parser1: P1, parser2: P2) -> impl Parser<'a, R1>
where
    P1: Parser<'a, R1>,
    P2: Parser<'a, R2>,
{
    map(pair(parser1, parser2), |(left, _right)| left)
}

fn right<'a, P1, P2, R1, R2>(parser1: P1, parser2: P2) -> impl Parser<'a, R2>
where
    P1: Parser<'a, R1>,
    P2: Parser<'a, R2>,
{
    map(pair(parser1, parser2), |(_left, right)| right)
}

fn match_range<'a, P, A>(parser: P, range: impl RangeBounds<usize>) -> impl Parser<'a, Vec<A>>
where
    P: Parser<'a, A>,
{
    move |mut input: &'a str| {
        let mut result = Vec::new();
        let min_occurs = match range.start_bound() {
            Bound::Unbounded => 0_usize,
            Bound::Included(i) => *i,
            Bound::Excluded(i) => *i + 1,
        };
        let max_occurs = match range.end_bound() {
            Bound::Unbounded => input.len(),
            Bound::Included(i) => *i,
            Bound::Excluded(i) => *i - 1,
        };
        while let Ok((next_input, item)) = parser.parse(input) {
            input = next_input;
            result.push(item);
            if result.len() == max_occurs {
                break;
            }
        }
        if (min_occurs <= result.len()) && (result.len() <= max_occurs) {
            Ok((input, result))
        } else {
            Err(input)
        }
    }
}

fn one_or_more<'a, P, A>(parser: P) -> impl Parser<'a, Vec<A>>
where
    P: Parser<'a, A>,
{
    match_range(parser, 1..)
}

fn zero_or_more<'a, P, A>(parser: P) -> impl Parser<'a, Vec<A>>
where
    P: Parser<'a, A>,
{
    match_range(parser, 0..)
}

fn match_literal<'a>(expected: &'static str) -> impl Parser<'a, ()> {
    move |input: &'a str| match input.starts_with(expected) {
        true => Ok((&input[expected.len()..], ())),
        false => Err(input),
    }
}

fn identifier(input: &str) -> ParseResult<String> {
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
    #[cfg(test)]
    mod match_literal_tests {
        use crate::match_literal;
        use crate::Parser;

        #[test]
        fn should_return_end_of_sentence() {
            let parse_joe = match_literal("Hello Joe!");
            assert_eq!(parse_joe.parse("Hello Joe!"), Ok(("", ())));
        }

        #[test]
        fn should_return_position_after_result() {
            let parse_joe = match_literal("Hello Joe!");
            assert_eq!(
                parse_joe.parse("Hello Joe! Hello Robert !"),
                Ok((" Hello Robert !", ())),
            );
        }

        #[test]
        fn should_return_error_with_input_if_not_found() {
            let parse_joe = match_literal("Hello Joe!");
            let parse_he = match_literal("He");
            assert_eq!(parse_joe.parse("Hello Mike!"), Err("Hello Mike!"),);
            assert_eq!(
                parse_he.parse("Should not He work"),
                Err("Should not He work"),
            );
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

    #[cfg(test)]
    mod pair_tests {
        use crate::identifier;
        use crate::match_literal;
        use crate::pair;
        use crate::Parser;

        #[test]
        fn pair_two_parsers() {
            let tag_opener = pair(match_literal("<"), identifier);
            assert_eq!(
                Ok(("/>", ((), "my-first-element".to_string()))),
                tag_opener.parse("<my-first-element/>")
            );
            assert_eq!(Err("oops"), tag_opener.parse("oops"));
            assert_eq!(Err("!oops"), tag_opener.parse("<!oops"));
        }
    }

    #[cfg(test)]
    mod right_left_tests {
        use crate::identifier;
        use crate::left;
        use crate::match_literal;
        use crate::right;
        use crate::Parser;

        #[test]
        fn right_combinator() {
            let tag_opener = right(match_literal("<"), identifier);
            assert_eq!(
                Ok(("/>", "my-first-element".to_string())),
                tag_opener.parse("<my-first-element/>")
            );
            assert_eq!(Err("oops"), tag_opener.parse("oops"));
            assert_eq!(Err("!oops"), tag_opener.parse("<!oops"));
        }

        #[test]
        fn left_combinator() {
            let tag_opener = left(match_literal("<"), identifier);
            assert_eq!(Ok(("/>", ())), tag_opener.parse("<my-first-element/>"));
            assert_eq!(Err("oops"), tag_opener.parse("oops"));
            assert_eq!(Err("!oops"), tag_opener.parse("<!oops"));
        }
    }

    #[cfg(test)]
    mod repetition_combinator_tests {
        use crate::match_literal;
        use crate::one_or_more;
        use crate::zero_or_more;
        use crate::Parser;

        #[test]
        fn one_or_more_combinator() {
            let parser = one_or_more(match_literal("ha"));
            assert_eq!(parser.parse("hahaha"), Ok(("", vec![(), (), ()])));
            assert_eq!(parser.parse("ahaha"), Err("ahaha"));
            assert_eq!(parser.parse(""), Err(""));
        }

        #[test]
        fn zero_or_more_combinator() {
            let parser = zero_or_more(match_literal("ha"));
            assert_eq!(parser.parse("hahaha"), Ok(("", vec![(), (), ()])));
            assert_eq!(parser.parse("ahah"), Ok(("ahah", vec![])));
            assert_eq!(parser.parse(""), Ok(("", vec![])));
        }
    }
}
