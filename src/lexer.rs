use std::fs::{self};

use regex::Regex;

fn read_file(filename: String) -> String {
    let filename = filename + ".i"; //accounting for the processed file name
    fs::read_to_string(&filename)
        .unwrap_or_else(|_| panic!("Could not read file {} from lexer", &filename))
}

pub fn lexer(filename: String) -> Vec<String> {
    let mut contents = read_file(filename);

    let token_regexes = vec![
        r"int\b",
        r"void\b",
        r"return\b",
        r"\(",
        r"\)",
        r"\{",
        r"\}",
        r";",
        r"[a-zA-Z_]\w*\b",
        r"[0-9]+\b",
        r"~",
        r"-",
        r"--",
        r"\+",
        r"\*",
        r"\/",
        r"%",
    ];

    let mut tokens: Vec<String> = Vec::new();

    while !contents.is_empty() {
        //trim whitespace at start
        contents = contents.trim_start().to_string();

        let mut valid_tokens = Vec::new();

        for regex_token in &token_regexes {
            let re = Regex::new(regex_token).unwrap();
            if let Some(mat) = re.find(&contents) {
                if contents.find(mat.as_str()).unwrap() != 0 {
                    continue;
                }
                valid_tokens.push(mat.as_str().to_string());
            }
        }

        if valid_tokens.is_empty() && !contents.is_empty() {
            panic!("invalid token found in {}", contents);
        }
        if valid_tokens.is_empty() {
            break;
        }

        let mut longest_token = valid_tokens.first().unwrap().to_string();
        for token in &valid_tokens {
            if token.len() > longest_token.len() {
                longest_token = token.clone();
            }
        }

        contents =
            String::from(&contents[contents.find(&longest_token).unwrap() + longest_token.len()..]);
        tokens.push(longest_token);
    }

    tokens
}
