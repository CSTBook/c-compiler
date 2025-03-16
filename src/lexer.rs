use std::fs::{self};

use regex::Regex;

fn read_file(filename: String) -> String {
    let filename = filename + ".i"; //accounting for the processed file name
    fs::read_to_string(&filename)
        .unwrap_or_else(|_| panic!("Could not read file {} from lexer", &filename))
}

//TODO: optimize the lexer
pub fn lexer(filename: String) -> Vec<String> {
    let mut contents = read_file(filename);

    let tokens = vec![
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
        r"&",
        r"\|",
        r"\^",
        r"<<",
        r">>",
    ];

    let token_regexes: Vec<Regex> = tokens.iter().map(|x| Regex::new(x).unwrap()).collect();

    let mut tokens: Vec<String> = Vec::new();

    while !contents.is_empty() {
        //trim whitespace at start
        contents = contents.trim_start().to_string();

        let mut token = String::new();

        for regex_token in &token_regexes {
            if let Some(mat) = regex_token.find(&contents) {
                if contents.find(mat.as_str()).unwrap() != 0 {
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

        contents =
            String::from(&contents[token.len()..]);
        tokens.push(token);
    }

    tokens
}
