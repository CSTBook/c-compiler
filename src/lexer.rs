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

    let token_regex = Regex::new(r"[a-zA-Z_]\w*\b\s*:|[a-zA-Z_]\w*\b|[0-9]+\b|\|\||&&|!=|<=|>=|==|>>=|<<=|>>|<<|int\b|void\b|return\b|\+\+|--|\+=|-=|\*=|\/=|%=|&=|\|=|\^=|if|else|=|>|<|!|\^|\||&|%|\/|\*|\+|-|~|;|\}|\{|\(|\)|<|>|=|\?|:").unwrap();

    let mut tokens: Vec<String> = Vec::new();

    while !contents.is_empty() {
        //trim whitespace at start
        contents = contents.trim_start().to_string();

        let mut token = String::new();

        for mat in token_regex.find_iter(&contents) {
            if mat.start() != 0 {
                continue;
            }
            let mat_str = mat.as_str();
            if mat_str.len() > token.len() {
                token = mat_str.to_string();
            }
        }

        if token.is_empty() {
            if !contents.is_empty() {
                panic!("Invalid token found in {}", contents)
            }
            break;
        }

        contents = String::from(&contents[token.len()..]);
        tokens.push(token);
    }

    tokens
        .iter()
        .map(|token| token.replace(|c: char| c.is_whitespace(), ""))
        .collect()

    // tokens
}
