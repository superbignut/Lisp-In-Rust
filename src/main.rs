use core::fmt;
use std::{collections::HashMap, io, num::ParseFloatError};

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

impl fmt::Display for RispExp {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let my_str = match self {
            RispExp::Symbol(s) => s.clone(),
            RispExp::Number(n) => n.to_string(),
            RispExp::List(list) => {
                let xs: Vec<String> = list.iter().map(|x| x.to_string()).collect();
                format!("({})", xs.join(","))
            }
            &RispExp::Func(_) => "Function{}".to_string(),
        };
        write!(f, "{}", my_str)
    }
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

fn eval(exp: &RispExp, env: &mut RispEnv) -> Result<RispExp, RispErr> {
    match exp {
        RispExp::Symbol(k) => env
            .data
            .get(k)
            .ok_or(RispErr::Reason(format!("unexpected symbol k='{}'", k)))
            .map(|x| x.clone()),

        RispExp::Number(_a) => Ok(exp.clone()),
        RispExp::List(list) => {
            let first_form = list
                .first()
                .ok_or(RispErr::Reason("require a non-empty list".to_string()))?;

            let arg_forms = &list[1..];

            let first_eval = eval(first_form, env)?;

            match first_eval {
                RispExp::Func(f) => {
                    let args_eval = arg_forms
                        .iter()
                        .map(|x| eval(x, env))
                        .collect::<Result<Vec<RispExp>, RispErr>>();
                    f(&args_eval?)
                }
                _ => Err(RispErr::Reason(
                    "first form must be a function.".to_string(),
                )),
            }
        }
        RispExp::Func(_) => Err(RispErr::Reason("unexpected form".to_string())),
    }
}

fn parse_eval(expr: String, env: &mut RispEnv) -> Result<RispExp, RispErr> {
    let (parse_exp, _) = parse(&tokenize(expr))?;
    let evaled_exp = eval(&parse_exp, env)?;
    Ok(evaled_exp)
}

fn slurp_expr() -> String {
    let mut expr = String::new();
    io::stdin()
        .read_line(&mut expr)
        .expect("Failed to read line");
    expr
}

fn main() {
    let env = &mut default_env();
    loop {
        println!("risp >");
        let expr = slurp_expr();

        match parse_eval(expr, env) {
            Ok(res) => println!("Answer is {}", res),
            Err(e) => match e {
                RispErr::Reason(msg) => println!("Error reason is {}", msg),
            },
        }
    }
}
