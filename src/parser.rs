use regex::Regex;

#[derive(PartialEq, Eq)]
enum AstNode {
    Program(Box<AstNode>),
    Function(String, Box<AstNode>),
    Return(Box<AstNode>),
    Constant(i32)
}

pub fn parser(mut tokens: Vec<String>) {
    let program = parse_program(&mut tokens);

    //check for extraneous tokens
    if tokens.len() > 0 {
        panic!("Extraneous tokens found: {:?}", tokens);
    }

    println!("{}", pretty_printer(program,0));
}

fn parse_program(tokens: &mut Vec<String>) -> AstNode {
    AstNode::Program(Box::new(parse_function(tokens)))
}

fn parse_function(tokens: &mut Vec<String>) -> AstNode {
    expect("int", tokens);
    let identifier = tokens.remove(0);
    
    //check if identifier is valid
    check_name(&identifier);

    expect("(", tokens);
    expect("void", tokens);
    expect(")", tokens);
    expect("{", tokens);
    let statement = parse_statement(tokens);
    expect("}", tokens);
    AstNode::Function(identifier, Box::new(statement))
}

fn check_name(name: &String) {
    let first = name.chars().nth(0).unwrap().to_string();
    let re = Regex::new(r"[A-Z]|[a-z]|_").unwrap();
    if !re.is_match(&first) {
        panic!("Invalid function name \"{}\"", name);
    }
}

fn parse_statement(tokens: &mut Vec<String>) -> AstNode{
    expect("return", tokens);
    let exp = AstNode::Constant(tokens.remove(0).parse::<i32>().unwrap());
    expect(";", tokens);
    AstNode::Return(Box::new(exp))
}

fn expect(expected: &str, tokens: &mut Vec<String>) -> String {
    let actual = tokens.get(0).unwrap();
    if actual != expected {
        panic!("Found {}, expected {}", actual, expected);
    }
    tokens.remove(0)
}

fn pretty_printer(node: AstNode, indent_level: usize) -> String {
    match node {
        AstNode::Program(func) => {
            format!("{}Program(\n{}{}\n{})",tabs(indent_level), tabs(indent_level), pretty_printer(*func,indent_level+1), tabs(indent_level))
        },
        AstNode::Function(name, statement) => {
            format!("{}Function(\n{}\tname=\"{}\"\n{}\tbody={}\n{})",tabs(indent_level),tabs(indent_level),name,tabs(indent_level),pretty_printer(*statement,indent_level+1),tabs(indent_level))
        },
        AstNode::Return(exp) => {
            format!("Return(\n{}{}\n{})",tabs(indent_level),pretty_printer(*exp, indent_level+1),tabs(indent_level+2))
        },
        AstNode::Constant(val) => {
            format!("{}Constant({})",tabs(indent_level),val)
        }
    }
}

fn tabs(indent_level: usize) -> String {
    "  ".repeat(indent_level)
}