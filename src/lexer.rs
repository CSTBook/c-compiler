use regex::Regex;
use std::fs::{self};

fn read_file(filename: String) -> String {
    let filename = filename + ".i"; //accounting for the processed file name
    fs::read_to_string(&filename)
        .unwrap_or_else(|_| panic!("Could not read file {} from lexer", &filename))
}

//TODO: optimize the lexer
pub fn lexer(filename: String) -> Vec<String> {
    let binding = read_file(filename);
    let mut contents = binding.as_str();

    //in descending length so as to reduce later computations
    let token_regexes = [
        r"^[a-zA-Z_]\w*\b\s*:",
        r"^[a-zA-Z_]\w*\b",
        r"^continue",
        r"^[0-9]+\b",
        r"^return\b",
        r"^default",
        r"^switch",
        r"^void\b",
        r"^break",
        r"^while",
        r"^int\b",
        r"^case",
        r"^else",
        r"^\+\+",
        r"^\|\|",
        r"^for",
        r"^>>=",
        r"^<<=",
        r"^\^=",
        r"^\|=",
        r"^\/=",
        r"^\*=",
        r"^\+=",
        r"^do",
        r"^\?",
        r"^if",
        r"^--",
        r"^&=",
        r"^%=",
        r"^-=",
        r"^>=",
        r"^<=",
        r"^!=",
        r"^==",
        r"^&&",
        r"^>>",
        r"^<<",
        r"^\^",
        r"^\|",
        r"^\/",
        r"^\*",
        r"^\+",
        r"^--",
        r"^\}",
        r"^\{",
        r"^\)",
        r"^\(",
        r"^:",
        r"^=",
        r"^>",
        r"^<",
        r"^!",
        r"^&",
        r"^%",
        r"^-",
        r"^~",
        r"^;",
    ];

    let regex_token = Regex::new(&token_regexes.join("|")).unwrap();

    let mut tokens: Vec<String> = Vec::new();

    loop {
        //trim whitespace at start
        contents = contents.trim_start();
        if contents.is_empty() {
            break;
        }

        let token;

        if let Some(mat) = regex_token.find(contents) {
            token = mat.as_str().to_string();
            contents = &contents[token.len()..];
            tokens.push(token.replace(|c: char| c.is_whitespace(), ""));
        } else {
            panic!("Invalid token found in {}", contents)
        }
    }

    tokens
}
