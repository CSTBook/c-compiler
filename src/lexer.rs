use std::fs::{self};

use regex::Regex;

fn read_file(filename: String) -> String {
    let filename = filename+".i"; //accounting for the processed file name
    let contents = fs::read_to_string(&filename).expect(format!("Could not read file {} from lexer", &filename).as_str());
    contents
}

pub fn lexer(filename: String) -> Vec<String>{
    let mut contents = read_file(filename);

    let token_regexes = vec![r"int\b", r"void\b", r"return\b", r"\(", r"\)", r"\{", r"\}", r";",r"[a-zA-Z_]\w*\b", r"[0-9]+\b"];

    let mut tokens: Vec<String> = Vec::new();

    'reader: while !contents.is_empty() {
        //trim whitespace at start
        contents = contents.trim_start().to_string();

        for regex_token in &token_regexes {
            let re = Regex::new(regex_token).unwrap();
            if let Some(mat) = re.find(&contents) {
                if contents.find(mat.as_str()).unwrap()!=0 {continue;}
                tokens.push(mat.as_str().to_string());
                contents = String::from(&contents[contents.find(mat.as_str()).unwrap()+mat.as_str().len()..]);
                continue 'reader;
            }
        }

        if !contents.is_empty() {panic!("invalid token found in {}", contents);}
    }

    tokens
}