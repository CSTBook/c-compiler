use regex::Regex;

pub enum AstNode {
    Program(Box<AstNode>),
    Function(String, Statement),
}
pub enum Statement {
    Return(Expression),
}
pub enum Expression {
    Constant(i32),
    Unary(UnaryParser, Box<Expression>),
    Binary(BinaryParser, Box<Expression>, Box<Expression>),
}
#[derive(PartialEq, Eq, Debug)]
pub enum UnaryParser {
    Complement,
    Negate,
}

pub enum BinaryParser {
    Add,
    Subtract,
    Multiply,
    Divide,
    Remainder,
}

pub fn parser(mut tokens: Vec<String>) -> AstNode {
    let program = parse_program(&mut tokens);

    //check for extraneous tokens
    if !tokens.is_empty() {
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
    AstNode::Function(identifier, statement)
}

fn check_name(name: &String) {
    let first = name.chars().next().unwrap().to_string();
    let re = Regex::new(r"[A-Z]|[a-z]|_").unwrap();
    if !re.is_match(&first) {
        panic!("Invalid function name \"{}\"", name);
    }
}

fn parse_statement(tokens: &mut Vec<String>) -> Statement {
    expect("return", tokens);
    let exp = parse_expression(tokens, 0);
    expect(";", tokens);
    Statement::Return(exp)
}

fn parse_factor(tokens: &mut Vec<String>) -> Expression {
    let next_token = tokens.first().unwrap();
    if next_token.parse::<i32>().is_ok() {
        Expression::Constant(tokens.remove(0).parse::<i32>().unwrap())
    } else if next_token == "-" || next_token == "~" {
        let operator = parse_unop(tokens.remove(0));
        let inner_exp = parse_factor(tokens);
        Expression::Unary(operator, Box::new(inner_exp))
    } else if next_token == "(" {
        tokens.remove(0);
        let inner_exp = parse_expression(tokens, 0);
        expect(")", tokens);
        inner_exp
    } else {
        //TODO: Add better compiler error
        panic!("Invalid code: {:?}", tokens);
    }
}

fn parse_expression(tokens: &mut Vec<String>, min_precedence: i32) -> Expression {
    let mut left = parse_factor(tokens);
    let mut next_token = tokens.first().unwrap().clone();
    while (next_token == "+" // only if we have a binary operator
        || next_token == "-"
        || next_token == "*"
        || next_token == "/"
        || next_token == "%")
        && get_precedence(next_token.to_string()) >= min_precedence
    //and the binary operator has higher precedence than the outer one
    {
        let operator = parse_binop(tokens);
        let right = parse_expression(tokens, get_precedence(next_token.to_string()) + 1); //so that we get left associativity
        left = Expression::Binary(operator, Box::new(left), Box::new(right));
        next_token = tokens.first().unwrap().clone();
    }

    left
}

fn get_precedence(token: String) -> i32 {
    match token.as_str() {
        "+" => 45,
        "-" => 45,
        "*" => 50,
        "/" => 50,
        "%" => 50,
        _ => unreachable!("Binary Operator Precedence Error"),
    }
}

fn parse_binop(tokens: &mut Vec<String>) -> BinaryParser {
    let binop = tokens.remove(0);
    match binop.as_str() {
        "+" => BinaryParser::Add,
        "-" => BinaryParser::Subtract,
        "*" => BinaryParser::Multiply,
        "/" => BinaryParser::Divide,
        "%" => BinaryParser::Remainder,
        _ => unreachable!("Binary Operator Parsing Error"),
    }
}

fn parse_unop(op: String) -> UnaryParser {
    if op == "-" {
        return UnaryParser::Negate;
    }
    UnaryParser::Complement
}

fn expect(expected: &str, tokens: &mut Vec<String>) -> String {
    let actual = tokens.first().unwrap();
    if actual != expected {
        panic!("Found {}, expected {}", actual, expected);
    }
    tokens.remove(0)
}

#[allow(dead_code)]
pub fn pretty_printer(node: &AstNode, indent_level: usize) -> String {
    match node {
        AstNode::Program(func) => {
            format!(
                "{}Program(\n{}{}\n{})",
                tabs(indent_level),
                tabs(indent_level),
                pretty_printer(func, indent_level + 1),
                tabs(indent_level)
            )
        }
        AstNode::Function(name, statement) => {
            format!(
                "{}Function(\n{}name=\"{}\"\n{}body={}\n{})",
                tabs(indent_level),
                tabs(indent_level + 1),
                name,
                tabs(indent_level + 1),
                pretty_printer_statement(statement, indent_level + 1),
                tabs(indent_level)
            )
        }
    }
}

fn pretty_printer_statement(statement: &Statement, indent_level: usize) -> String {
    match statement {
        Statement::Return(expression) => format!(
            "Return(\n{}\n{})",
            pretty_printer_expr(expression, indent_level + 1),
            tabs(indent_level)
        ),
    }
}

fn pretty_printer_expr(expression: &Expression, indent_level: usize) -> String {
    match expression {
        Expression::Constant(val) => {
            format!("{}Constant({})", tabs(indent_level), val)
        }
        Expression::Unary(unary, ast_node) => {
            let mut output = format!("{}Unary(", tabs(indent_level));
            output += match unary {
                UnaryParser::Complement => "Complement",
                UnaryParser::Negate => "Negate",
            };
            output += &format!(")\n{}", pretty_printer_expr(ast_node, indent_level + 1));
            output
        }
        Expression::Binary(binop, left, right) => {
            format!(
                "{}Binary(\n{}\n{}\n{}\n{})",
                tabs(indent_level),
                pretty_printer_binop(binop, indent_level + 1),
                pretty_printer_expr(left, indent_level + 1),
                pretty_printer_expr(right, indent_level + 1),
                tabs(indent_level)
            )
        }
    }
}

fn pretty_printer_binop(binop: &BinaryParser, indent_level: usize) -> String {
    match binop {
        BinaryParser::Add => format!("{}Add,", tabs(indent_level)),
        BinaryParser::Subtract => format!("{}Subtract,", tabs(indent_level)),
        BinaryParser::Multiply => format!("{}Multiply,", tabs(indent_level)),
        BinaryParser::Divide => format!("{}Divide,", tabs(indent_level)),
        BinaryParser::Remainder => format!("{}Remainder,", tabs(indent_level)),
    }
}

#[allow(dead_code)]
fn tabs(indent_level: usize) -> String {
    "  ".repeat(indent_level)
}
