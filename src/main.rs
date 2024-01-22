use std::{collections::HashMap, num::ParseFloatError};

// Varibale type.
#[derive(Clone)]
enum RispExp {
    Symbol(String),
    Number(f64),
    List(Vec<RispExp>),
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

fn main() {
    let tokens = tokenize(String::from("(+ 10 5)"));

    let mut x = [1, 2, 3];

    let y = &mut x[..];

    for n in y {
        *n += 1;
        println!("{}", n);

        println!("{}", 1123);
    }
}
//Todo: what is &[String], slice?

//Todo:
