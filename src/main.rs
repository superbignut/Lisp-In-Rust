use core::fmt;
use std::{clone, collections::HashMap, fmt::format, io, num::ParseFloatError, rc::Rc};

macro_rules! ensure_tonicity {
    ($check_fn:expr) => {{
        |args: &[RispExp]| -> Result<RispExp, RispErr> {
            let floats = parse_list_of_floats(args)?;
            let first_num = floats
                .first()
                .ok_or(RispErr::Reason("expecteed at least one number".to_string()))?;
            let rest = &floats[1..];
            fn f(prev: &f64, xs: &[f64]) -> bool {
                match xs.first() {
                    Some(x) => $check_fn(prev, x) && f(x, &xs[1..]),
                    None => true,
                }
            }
            Ok(RispExp::Bool(f(first_num, rest)))
        }
    }};
}

#[derive(Clone)]
struct RispLambda {
    params_exp: Rc<RispExp>,
    body_exp: Rc<RispExp>,
}

// Varibale type.
#[derive(Clone)]
enum RispExp {
    Bool(bool),
    Symbol(String),
    Number(f64),
    List(Vec<RispExp>),
    Func(fn(&[RispExp]) -> Result<RispExp, RispErr>),
    Lambda(RispLambda),
}

// Error type.
#[derive(Debug)]
enum RispErr {
    Reason(String),
}

// Environment type.
#[derive(Clone)]
struct RispEnv<'a> {
    data: HashMap<String, RispExp>,
    outer: Option<&'a RispEnv<'a>>,
}

impl fmt::Display for RispExp {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let my_str = match self {
            RispExp::Bool(a) => a.to_string(),
            RispExp::Symbol(s) => s.clone(),
            RispExp::Number(n) => n.to_string(),
            RispExp::List(list) => {
                let xs: Vec<String> = list.iter().map(|x| x.to_string()).collect();
                format!("({})", xs.join(","))
            }
            &RispExp::Func(_) => "Function{}".to_string(),
            RispExp::Lambda(_) => "Lambda {}".to_string(),
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
    match token {
        "true" => RispExp::Bool(true),
        "false" => RispExp::Bool(false),
        _ => {
            let potential_float: Result<f64, ParseFloatError> = token.parse();
            match potential_float {
                Ok(v) => RispExp::Number(v),
                Err(_) => RispExp::Symbol(token.to_string()),
            }
        }
    }
}

fn default_env<'a>() -> RispEnv<'a> {
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

    data.insert(
        "=".to_string(),
        RispExp::Func(ensure_tonicity!(|a, b| a == b)),
    );
    data.insert(
        ">".to_string(),
        RispExp::Func(ensure_tonicity!(|a, b| a > b)),
    );
    data.insert(
        ">=".to_string(),
        RispExp::Func(ensure_tonicity!(|a, b| a >= b)),
    );
    data.insert(
        "<".to_string(),
        RispExp::Func(ensure_tonicity!(|a, b| a < b)),
    );
    data.insert(
        "<=".to_string(),
        RispExp::Func(ensure_tonicity!(|a, b| a <= b)),
    );

    RispEnv { data, outer: None }
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

fn eval_if_args(arg_forms: &[RispExp], env: &mut RispEnv) -> Result<RispExp, RispErr> {
    let test_form = arg_forms
        .first()
        .ok_or(RispErr::Reason("expected test form".to_string()))?;

    let test_eval = eval(test_form, env)?;

    match test_eval {
        RispExp::Bool(b) => {
            let form_idx = if b { 1 } else { 2 };
            let rest_form = arg_forms
                .get(form_idx)
                .ok_or(RispErr::Reason(format!("expected form idx={}", form_idx)))?;
            let rest_eval = eval(rest_form, env);
            rest_eval
        }
        _ => Err(RispErr::Reason(format!(
            "unexpected test form={}",
            test_form.to_string()
        ))),
    }
}

fn eval_def_args(arg_forms: &[RispExp], env: &mut RispEnv) -> Result<RispExp, RispErr> {
    let first_form = arg_forms
        .first()
        .ok_or(RispErr::Reason("expected first form".to_string()))?;

    let first_str = match first_form {
        RispExp::Symbol(s) => Ok(s.clone()),
        _ => Err(RispErr::Reason(
            "expected first form to be a symbol".to_string(),
        )),
    }?;

    let second_form = arg_forms
        .get(1)
        .ok_or(RispErr::Reason("expected second form".to_string()))?;

    if arg_forms.len() > 2 {
        return Err(RispErr::Reason("def can only have two forms".to_string()));
    }

    let second_eval = eval(second_form, env)?;

    env.data.insert(first_str, second_eval);

    Ok(first_form.clone())
}

fn eval_lambda_args(arg_forms: &[RispExp]) -> Result<RispExp, RispErr> {
    let params_exp = arg_forms
        .first()
        .ok_or(RispErr::Reason("expected args form".to_string()))?;

    let body_exp = arg_forms
        .get(1)
        .ok_or(RispErr::Reason("expected second form".to_string()))?;

    if arg_forms.len() > 2 {
        return Err(RispErr::Reason(
            "fn defition can only have two forms.".to_string(),
        ));
    }

    Ok(RispExp::Lambda(RispLambda {
        params_exp: Rc::new(params_exp.clone()),
        body_exp: Rc::new(body_exp.clone()),
    }))
}

fn eval_built_in_form(
    exp: &RispExp,
    arg_forms: &[RispExp],
    env: &mut RispEnv,
) -> Option<Result<RispExp, RispErr>> {
    match exp {
        RispExp::Symbol(s) => match s.as_ref() {
            "if" => Some(eval_if_args(arg_forms, env)),
            "def" => Some(eval_def_args(arg_forms, env)),
            "fn" => Some(eval_lambda_args(arg_forms)),
            _ => None,
        },
        _ => None,
    }
}

fn env_get(k: &str, env: &RispEnv) -> Option<RispExp> {
    match env.data.get(k) {
        Some(exp) => Some(exp.clone()),
        None => match &env.outer {
            Some(outer_env) => env_get(k, &outer_env),
            None => None,
        },
    }
}

fn eval_forms(arg_forms: &[RispExp], env: &mut RispEnv) -> Result<Vec<RispExp>, RispErr> {
    arg_forms.iter().map(|x| eval(x, env)).collect()
}

fn env_for_lambda<'a>(
    params: Rc<RispExp>,
    arg_forms: &[RispExp],
    outer_env: &'a mut RispEnv,
) -> Result<RispEnv<'a>, RispErr> {
    //    let ks = parse_list_of_symbol_strings(params)?;
    let ks = parse_list_of_symbol_strings(params)?;
    if ks.len() != arg_forms.len() {
        return Err(RispErr::Reason(format!(
            "expected {} arguments, got {}",
            ks.len(),
            arg_forms.len()
        )));
    }
    let vs = eval_forms(arg_forms, outer_env)?;
    let mut data: HashMap<String, RispExp> = HashMap::new();
    for (k, v) in ks.iter().zip(vs.iter()) {
        data.insert(k.clone(), v.clone());
    }
    Ok(RispEnv {
        data,
        outer: Some(outer_env),
    })
}

fn parse_list_of_symbol_strings(form: Rc<RispExp>) -> Result<Vec<String>, RispErr> {
    let list = match form.as_ref() {
        RispExp::List(s) => Ok(s.clone()),
        _ => Err(RispErr::Reason(
            "expeced args form to be a list".to_string(),
        )),
    }?;
    list.iter()
        .map(|x| match x {
            RispExp::Symbol(s) => Ok(s.clone()),
            _ => Err(RispErr::Reason(
                "expected symbols in the arguments list".to_string(),
            )),
        })
        .collect()
}

fn eval(exp: &RispExp, env: &mut RispEnv) -> Result<RispExp, RispErr> {
    match exp {
        RispExp::Symbol(k) => env_get(k, env)
            .ok_or(RispErr::Reason(format!("unexpected symbol k='{}'", k)))
            .map(|x| x.clone()),

        RispExp::Number(_a) => Ok(exp.clone()),
        RispExp::List(list) => {
            let first_form = list
                .first()
                .ok_or(RispErr::Reason("require a non-empty list".to_string()))?;

            let arg_forms = &list[1..];

            match eval_built_in_form(first_form, arg_forms, env) {
                Some(res) => res,
                None => {
                    let first_eval = eval(first_form, env)?;

                    match first_eval {
                        RispExp::Func(f) => f(&eval_forms(arg_forms, env)?),
                        RispExp::Lambda(lambda) => {
                            let new_env = &mut env_for_lambda(lambda.params_exp, arg_forms, env)?;
                            eval(&lambda.body_exp, new_env)
                        }
                        _ => Err(RispErr::Reason(
                            "first form must be a function.".to_string(),
                        )),
                    }
                }
            }
        }
        RispExp::Bool(_a) => Ok(exp.clone()),
        RispExp::Func(_) => Err(RispErr::Reason("unexpected form".to_string())),
        RispExp::Lambda(_) => Err(RispErr::Reason("unexpected form".to_string())),
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
