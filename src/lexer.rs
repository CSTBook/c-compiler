use regex::Regex;
use std::fs::{self};

fn read_file(filename: String) -> String {
    let filename = filename + ".i"; //accounting for the processed file name
    fs::read_to_string(&filename)
        .unwrap_or_else(|_| panic!("Could not read file {} from lexer", &filename))
}

//TODO: optimize the lexer
pub fn lexer(filename: String) -> Vec<String> {
    let mut contents = read_file(filename);

    let token_regexes = [
        Regex::new(r"int\b").unwrap(),
        Regex::new(r"void\b").unwrap(),
        Regex::new(r"return\b").unwrap(),
        Regex::new(r"\(").unwrap(),
        Regex::new(r"\)").unwrap(),
        Regex::new(r"\{").unwrap(),
        Regex::new(r"\}").unwrap(),
        Regex::new(r";").unwrap(),
        Regex::new(r"[a-zA-Z_]\w*\b").unwrap(),
        Regex::new(r"[0-9]+\b").unwrap(),
        Regex::new(r"~").unwrap(),
        Regex::new(r"-").unwrap(),
        Regex::new(r"--").unwrap(),
        Regex::new(r"\+").unwrap(),
        Regex::new(r"\*").unwrap(),
        Regex::new(r"\/").unwrap(),
        Regex::new(r"%").unwrap(),
        Regex::new(r"&").unwrap(),
        Regex::new(r"\|").unwrap(),
        Regex::new(r"\^").unwrap(),
        Regex::new(r"<<").unwrap(),
        Regex::new(r">>").unwrap(),
        Regex::new(r"!").unwrap(),
        Regex::new(r"&&").unwrap(),
        Regex::new(r"\|\|").unwrap(),
        Regex::new(r"==").unwrap(),
        Regex::new(r"!=").unwrap(),
        Regex::new(r"<").unwrap(),
        Regex::new(r">").unwrap(),
        Regex::new(r"<=").unwrap(),
        Regex::new(r">=").unwrap(),
        Regex::new(r"=").unwrap(),
        Regex::new(r"\+=").unwrap(),
        Regex::new(r"-=").unwrap(),
        Regex::new(r"\*=").unwrap(),
        Regex::new(r"\/=").unwrap(),
        Regex::new(r"%=").unwrap(),
        Regex::new(r"&=").unwrap(),
        Regex::new(r"\|=").unwrap(),
        Regex::new(r"\^=").unwrap(),
        Regex::new(r"<<=").unwrap(),
        Regex::new(r">>=").unwrap(),
        Regex::new(r"--").unwrap(),
        Regex::new(r"\+\+").unwrap(),
        Regex::new(r"if").unwrap(),
        Regex::new(r"else").unwrap(),
        Regex::new(r"\?").unwrap(),
        Regex::new(r":").unwrap(),
        Regex::new(r"[a-zA-Z_]\w*\b\s*:").unwrap(),
        Regex::new(r"do").unwrap(),
        Regex::new(r"for").unwrap(),
        Regex::new(r"while").unwrap(),
        Regex::new(r"break").unwrap(),
        Regex::new(r"continue").unwrap(),
    ];

    let mut tokens: Vec<String> = Vec::new();

    while !contents.is_empty() {
        //trim whitespace at start
        contents = contents.trim_start().to_string();

        let mut token = String::new();

        for regex_token in &token_regexes {
            if let Some(mat) = regex_token.find(&contents) {
                if mat.start() != 0 {
                    continue;
                }
                if mat.as_str().len() > token.len() {
                    token = mat.as_str().to_string();
                }
            }
        }

        if token.is_empty() {
            if !contents.is_empty() {
                panic!("Invalid token found in {}", contents)
            }
            break;
        }

        contents = String::from(&contents[token.len()..]);
        tokens.push(token.replace(|c: char| c.is_whitespace(), ""));
    }

    tokens
}
