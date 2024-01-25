use std::{arch::x86_64, collections::HashMap, num::ParseFloatError};

// Varibale type.
#[derive(Clone)]
enum RispExp {
    Symbol(String),
    Number(f64),
    List(Vec<RispExp>),
    Func(fn(&[RispExp]) -> Result<RispExp, RispErr>),
}

// Error type.
#[derive(Debug)]
enum RispErr {
    Reason(String),
}

// Environment type.
#[derive(Clone)]
struct RispEnv {
    data: HashMap<String, RispExp>,
}

fn tokenize(expr: String) -> Vec<String> {
    expr.replace('(', " ( ")
        .replace(')', " ) ")
        .split_whitespace()
        .map(|x| x.to_string())
        .collect()
}

fn parse(tokens: &[String]) -> Result<(RispExp, &[String]), RispErr> {
    let (first_token, rest_tokens) = tokens
        .split_first()
        .ok_or(RispErr::Reason("could not get tokens".to_string()))?;

    match &first_token[..] {
        "(" => read_seq(rest_tokens),
        ")" => Err(RispErr::Reason("unexpected `)`".to_string())),
        _ => Ok((parse_atom(first_token), rest_tokens)),
    }
}

fn read_seq(tokens: &[String]) -> Result<(RispExp, &[String]), RispErr> {
    let mut temp_risp_exp: Vec<RispExp> = vec![];
    let mut input_tokens_seq = tokens;
    loop {
        let (first_token_in_seq, rest_tokens_in_seq) = input_tokens_seq
            .split_first()
            .ok_or(RispErr::Reason("Could not find closing.".to_string()))?;

        if first_token_in_seq == ")" {
            return Ok((RispExp::List(temp_risp_exp), rest_tokens_in_seq));
        }

        let (next_exp, new_input_tokens_seq) = parse(input_tokens_seq)?;

        temp_risp_exp.push(next_exp);

        input_tokens_seq = new_input_tokens_seq;
    }
}

fn parse_atom(token: &str) -> RispExp {
    let potential_float: Result<f64, ParseFloatError> = token.parse();
    match potential_float {
        Ok(v) => RispExp::Number(v),
        Err(_) => RispExp::Symbol(token.to_string()),
    }
}

fn default_env() -> RispEnv {
    let mut data: HashMap<String, RispExp> = HashMap::new();
    data.insert(
        "+".to_string(),
        RispExp::Func(|args: &[RispExp]| -> Result<RispExp, RispErr> {
            let sum = parse_list_of_floats(args)?
                .iter()
                .fold(0.0, |sum, a| sum + a);
            Ok(RispExp::Number(sum))
        }),
    );
    data.insert(
        "-".to_string(),
        RispExp::Func(|args| {
            let floats = parse_list_of_floats(args)?;
            let first_num = floats
                .first()
                .ok_or(RispErr::Reason("expecteed at least one number".to_string()))?;
            let sum_of_rest = floats[1..].iter().fold(0.0, |sum, a| sum + a);

            Ok(RispExp::Number(first_num - sum_of_rest))
        }),
    );

    RispEnv { data }
}

fn parse_list_of_floats(args: &[RispExp]) -> Result<Vec<f64>, RispErr> {
    args.iter().map(|x| parse_single_float(x)).collect()
}

fn parse_single_float(exp: &RispExp) -> Result<f64, RispErr> {
    match exp {
        RispExp::Number(num) => Ok(*num),
        _ => Err(RispErr::Reason("Expected a number".to_string())),
    }
}

fn main() {
    let tokens = tokenize(String::from("(+ 10 5)"));
    let (risp, rest) = parse(&tokens).unwrap();
    println!("{}", rest.len());

    let test1 = RispExp::Number(123f64);
    let test2 = RispExp::Number(123f64);
    let test3 = RispExp::Number(123f64);

    let newt = &[test1, test2, test3];
}
