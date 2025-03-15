use regex::Regex;

pub enum AstNode {
    Program(Box<AstNode>),
    Function(String, Statement)
}
pub enum Statement {
    Return(Expression)
}
pub enum Expression {
    Constant(i32),
    Unary(Unary, Box<Expression>),
}
#[derive(PartialEq, Eq,Debug)]
pub enum Unary {
    Complement,
    Negate,
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
    let exp = parse_expression(tokens);
    expect(";", tokens);
    Statement::Return(exp)
}

fn parse_expression(tokens: &mut Vec<String>) -> Expression {
    let next_token = tokens.first().unwrap();
    if next_token.parse::<i32>().is_ok() {
        Expression::Constant(tokens.remove(0).parse::<i32>().unwrap())
    } else if next_token == "-" || next_token == "~" {
        let operator = parse_unop(tokens.remove(0));
        let inner_exp = parse_expression(tokens);
        Expression::Unary(operator, Box::new(inner_exp))
    } else if next_token == "(" {
        tokens.remove(0);
        let inner_exp = parse_expression(tokens);
        expect(")", tokens);
        inner_exp
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
        Statement::Return(expression) => format!("Return(\n{}\n{})",pretty_printer_expr(expression, indent_level+1),tabs(indent_level)),
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
            Unary::Complement => "Complement",
            Unary::Negate => "Negate",
        };
        output += &format!(")\n{}", pretty_printer_expr(ast_node, indent_level));
        output
    }
    }
}

#[allow(dead_code)]
fn tabs(indent_level: usize) -> String {
    "  ".repeat(indent_level)
}
