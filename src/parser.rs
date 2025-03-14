use regex::Regex;

#[derive(PartialEq, Eq)]
pub enum AstNode {
    Program(Box<AstNode>),
    Function(String, Box<AstNode>),
    Return(Box<AstNode>),
    Constant(i32),
    Unary(Unary, Box<AstNode>)
}
#[derive(PartialEq, Eq)]
pub enum Unary {
    Complement,
    Negate
}

pub fn parser(mut tokens: Vec<String>) -> AstNode{
    let program = parse_program(&mut tokens);

    //check for extraneous tokens
    if tokens.len() > 0 {
        panic!("Extraneous tokens found: {:?}", tokens);
    }

    program
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
    let exp = parse_expression(tokens);
    expect(";", tokens);
    AstNode::Return(Box::new(exp))
}

fn parse_expression(tokens: &mut Vec<String>) -> AstNode {
    let next_token = tokens.get(0).unwrap();
    if next_token.parse::<i32>().is_ok() {
        return AstNode::Constant(tokens.remove(0).parse::<i32>().unwrap());
    } else if next_token == "-" || next_token == "~" {
        let operator = parse_unop(tokens.remove(0));
        let inner_exp = parse_expression(tokens);
        return AstNode::Unary(operator, Box::new(inner_exp));
    } else if next_token == "(" {
        tokens.remove(0);
        let inner_exp = parse_expression(tokens);
        expect(")", tokens);
        return inner_exp;
    } else {
        //TODO: Add better compiler error
        panic!("Invalid code: {:?}", tokens);
    }
}

fn parse_unop(op: String) -> Unary {
    if op == "-" {
        return Unary::Negate;
    }
    Unary::Complement
}

fn expect(expected: &str, tokens: &mut Vec<String>) -> String {
    let actual = tokens.get(0).unwrap();
    if actual != expected {
        panic!("Found {}, expected {}", actual, expected);
    }
    tokens.remove(0)
}

#[allow(dead_code)]
pub fn pretty_printer(node: &AstNode, indent_level: usize) -> String {
    match node {
        AstNode::Program(func) => {
                        format!("{}Program(\n{}{}\n{})",tabs(indent_level), tabs(indent_level), pretty_printer(func,indent_level+1), tabs(indent_level))
            },
        AstNode::Function(name, statement) => {
                format!("{}Function(\n{}name=\"{}\"\n{}body={}\n{})",tabs(indent_level),tabs(indent_level+1),name,tabs(indent_level+1),pretty_printer(statement,indent_level+1),tabs(indent_level))
            },
        AstNode::Return(exp) => {
                format!("Return(\n{}\n{})", pretty_printer(exp, indent_level+1),tabs(indent_level))
            },
        AstNode::Constant(val) => {
                format!("{}Constant({})",tabs(indent_level),val)
            }
        AstNode::Unary(unary, ast_node) => {
            let mut output = format!("{}Unary(",tabs(indent_level));
            output += match unary {
                Unary::Complement => "Complement",
                Unary::Negate => "Negate",
            };
            output += &format!(")\n{}",pretty_printer(ast_node, indent_level));
            output
        },
    }
}

#[allow(dead_code)]
fn tabs(indent_level: usize) -> String {
    "  ".repeat(indent_level)
}