use regex::Regex;

pub struct Program {
    pub function: Function,
}

pub struct Function {
    pub name: String,
    pub body: Vec<BlockItem>,
}

#[derive(Clone)]
pub enum BlockItem {
    Statement(Statement),
    Declaration(Declaration),
}

#[derive(Clone)]
pub struct Declaration {
    pub name: String,
    pub init: Option<Expression>, //option in case of no init
}

#[derive(Clone)]
pub enum Statement {
    Return(Expression),
    Expression(Expression),
    Null,
}

#[derive(Clone)]
pub enum Expression {
    Constant(i32),
    Var(String),
    Unary(UnaryParser, Box<Expression>),
    Binary(BinaryParser, Box<Expression>, Box<Expression>),
    Assignment(Box<Expression>, Box<Expression>), // var = exp
}
#[derive(PartialEq, Eq, Debug, Clone)]
pub enum UnaryParser {
    Complement,
    Negate,
    Not,
}
#[derive(Clone)]
pub enum BinaryParser {
    Add,
    Subtract,
    Multiply,
    Divide,
    Remainder,
    BitwiseAnd,
    BitwiseOr,
    BitwiseXor,
    BitwiseLeftShift,
    BitwiseRightShift,
    And,
    Or,
    Equal,
    NotEqual,
    LessThan,
    LessOrEqual,
    GreaterThan,
    GreaterOrEqual,
}

pub fn parser(mut tokens: Vec<String>) -> Program {
    let program = parse_program(&mut tokens);

    //check for extraneous tokens
    if !tokens.is_empty() {
        panic!("Extraneous tokens found: {:?}", tokens);
    }

    program
}

fn parse_program(tokens: &mut Vec<String>) -> Program {
    Program {
        function: parse_function(tokens),
    }
}

fn parse_function(tokens: &mut Vec<String>) -> Function {
    expect("int", tokens);
    let identifier = tokens.remove(0);

    //check if identifier is valid
    check_name(&identifier);

    expect("(", tokens);
    expect("void", tokens);
    expect(")", tokens);
    expect("{", tokens);

    let mut function_body: Vec<BlockItem> = Vec::new();
    while tokens.first().unwrap() != "}" {
        function_body.push(parse_block(tokens));
        // println!("{:?}",tokens);
    }

    expect("}", tokens);

    Function {
        name: identifier,
        body: function_body,
    }
}

fn check_name(name: &String) {
    let first = name.chars().next().unwrap().to_string();
    let re = Regex::new(r"[A-Z]|[a-z]|_").unwrap();
    let keywords = ["int", "return", "void"];
    if !re.is_match(&first) || keywords.contains(&name.as_str()) {
        panic!("Invalid name \"{}\"", name);
    }
}

fn parse_block(tokens: &mut Vec<String>) -> BlockItem {
    match tokens.first().unwrap().as_str() {
        "int" => BlockItem::Declaration(parse_declaration(tokens)),
        _ => BlockItem::Statement(parse_statement(tokens)),
    }
}

fn parse_declaration(tokens: &mut Vec<String>) -> Declaration {
    expect("int", tokens); //hard coded int for now
    let name = tokens.remove(0);
    check_name(&name);
    if tokens.first().unwrap().as_str() == "=" {
        tokens.remove(0);
        let exp = parse_expression(tokens, 0);
        expect(";", tokens);
        return Declaration {
            name,
            init: Some(exp),
        };
    }
    expect(";", tokens);
    Declaration { name, init: None }
}

fn parse_statement(tokens: &mut Vec<String>) -> Statement {
    let next_token = tokens.first().unwrap();
    match next_token.as_str() {
        "return" => {
            tokens.remove(0);
            let exp = parse_expression(tokens, 0);
            expect(";", tokens);
            Statement::Return(exp)
        }
        ";" => {
            tokens.remove(0);
            Statement::Null
        }
        _ => {
            let exp = parse_expression(tokens, 0);
            expect(";", tokens);
            Statement::Expression(exp)
        }
    }
}

fn parse_expression(tokens: &mut Vec<String>, min_precedence: i32) -> Expression {
    let mut left = parse_factor(tokens);
    let mut next_token = tokens.first().unwrap().clone();
    let binary_tokens = [
        "+", "-", "*", "/", "%", "|", "%", "|", "^", "&", ">>", "<<", "&&", "||", "==", "!=", ">",
        "<", ">=", "<=", "=", "+=", "-=", "*=", "/=", "%=", "&=", "|=", "^=", "<<=", ">>=",
    ];
    while binary_tokens.contains(&next_token.as_str())
        && get_precedence(next_token.to_string()) >= min_precedence
    //and the binary operator has higher precedence than the outer one
    {
        match next_token.as_str() {
            "=" => {
                tokens.remove(0);
                let right = parse_expression(tokens, get_precedence(next_token.to_string())); //so that we get right associativity
                left = Expression::Assignment(Box::new(left), Box::new(right));
            }
            "+=" => {
                tokens.remove(0);
                let right = parse_expression(tokens, get_precedence(next_token.to_string())); //so that we get right associativity
                left = Expression::Assignment(
                    Box::new(left.clone()),
                    Box::new(Expression::Binary(
                        BinaryParser::Add,
                        Box::new(left),
                        Box::new(right),
                    )),
                );
            }
            "-=" => {
                tokens.remove(0);
                let right = parse_expression(tokens, get_precedence(next_token.to_string())); //so that we get right associativity
                left = Expression::Assignment(
                    Box::new(left.clone()),
                    Box::new(Expression::Binary(
                        BinaryParser::Subtract,
                        Box::new(left),
                        Box::new(right),
                    )),
                );
            }
            "/=" => {
                tokens.remove(0);
                let right = parse_expression(tokens, get_precedence(next_token.to_string())); //so that we get right associativity
                left = Expression::Assignment(
                    Box::new(left.clone()),
                    Box::new(Expression::Binary(
                        BinaryParser::Divide,
                        Box::new(left),
                        Box::new(right),
                    )),
                );
            }
            "*=" => {
                tokens.remove(0);
                let right = parse_expression(tokens, get_precedence(next_token.to_string())); //so that we get right associativity
                left = Expression::Assignment(
                    Box::new(left.clone()),
                    Box::new(Expression::Binary(
                        BinaryParser::Multiply,
                        Box::new(left),
                        Box::new(right),
                    )),
                );
            }
            "%=" => {
                tokens.remove(0);
                let right = parse_expression(tokens, get_precedence(next_token.to_string())); //so that we get right associativity
                left = Expression::Assignment(
                    Box::new(left.clone()),
                    Box::new(Expression::Binary(
                        BinaryParser::Remainder,
                        Box::new(left),
                        Box::new(right),
                    )),
                );
            }
            "&=" => {
                tokens.remove(0);
                let right = parse_expression(tokens, get_precedence(next_token.to_string())); //so that we get right associativity
                left = Expression::Assignment(
                    Box::new(left.clone()),
                    Box::new(Expression::Binary(
                        BinaryParser::BitwiseAnd,
                        Box::new(left),
                        Box::new(right),
                    )),
                );
            }
            "|=" => {
                tokens.remove(0);
                let right = parse_expression(tokens, get_precedence(next_token.to_string())); //so that we get right associativity
                left = Expression::Assignment(
                    Box::new(left.clone()),
                    Box::new(Expression::Binary(
                        BinaryParser::BitwiseOr,
                        Box::new(left),
                        Box::new(right),
                    )),
                );
            }
            "^=" => {
                tokens.remove(0);
                let right = parse_expression(tokens, get_precedence(next_token.to_string())); //so that we get right associativity
                left = Expression::Assignment(
                    Box::new(left.clone()),
                    Box::new(Expression::Binary(
                        BinaryParser::BitwiseXor,
                        Box::new(left),
                        Box::new(right),
                    )),
                );
            }
            "<<=" => {
                tokens.remove(0);
                let right = parse_expression(tokens, get_precedence(next_token.to_string())); //so that we get right associativity
                left = Expression::Assignment(
                    Box::new(left.clone()),
                    Box::new(Expression::Binary(
                        BinaryParser::BitwiseLeftShift,
                        Box::new(left),
                        Box::new(right),
                    )),
                );
            }
            ">>=" => {
                tokens.remove(0);
                let right = parse_expression(tokens, get_precedence(next_token.to_string())); //so that we get right associativity
                left = Expression::Assignment(
                    Box::new(left.clone()),
                    Box::new(Expression::Binary(
                        BinaryParser::BitwiseRightShift,
                        Box::new(left),
                        Box::new(right),
                    )),
                );
            }
            _ => {
                let operator = parse_binop(tokens);
                let right = parse_expression(tokens, get_precedence(next_token.to_string()) + 1); //so that we get left associativity
                left = Expression::Binary(operator, Box::new(left), Box::new(right));
            }
        }
        next_token = tokens.first().unwrap().clone();
    }

    left
}

fn parse_factor(tokens: &mut Vec<String>) -> Expression {
    let next_token = tokens.first().unwrap();
    if next_token.parse::<i32>().is_ok() {
        Expression::Constant(tokens.remove(0).parse::<i32>().unwrap())
    } else if next_token == "-" || next_token == "~" || next_token == "!" {
        let operator = parse_unop(tokens.remove(0));
        let inner_exp = parse_factor(tokens);
        Expression::Unary(operator, Box::new(inner_exp))
    } else if next_token == "(" {
        tokens.remove(0);
        let inner_exp = parse_expression(tokens, 0);
        expect(")", tokens);
        inner_exp
    } else {
        //for the time being, until i implement semantic analysis
        check_name(next_token);
        Expression::Var(tokens.remove(0))
    }
}

fn get_precedence(token: String) -> i32 {
    match token.as_str() {
        "=" | "+=" | "-=" | "*=" | "/=" | "%=" | "&=" | "|=" | "^=" | ">>=" | "<<=" => 1,
        "||" => 3,
        "&&" => 4,
        "|" => 5,
        "^" => 6,
        "&" => 7,
        "==" | "!=" => 8,
        "<" | "<=" | ">" | ">=" => 9,
        "<<" | ">>" => 10,
        "+" | "-" => 11,
        "*" | "/" | "%" => 12,
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
        "|" => BinaryParser::BitwiseOr,
        "^" => BinaryParser::BitwiseXor,
        "&" => BinaryParser::BitwiseAnd,
        "<<" => BinaryParser::BitwiseLeftShift,
        ">>" => BinaryParser::BitwiseRightShift,
        "&&" => BinaryParser::And,
        "||" => BinaryParser::Or,
        "==" => BinaryParser::Equal,
        "!=" => BinaryParser::NotEqual,
        ">" => BinaryParser::GreaterThan,
        ">=" => BinaryParser::GreaterOrEqual,
        "<" => BinaryParser::LessThan,
        "<=" => BinaryParser::LessOrEqual,
        _ => unreachable!("Binary Operator Parsing Error"),
    }
}

fn parse_unop(op: String) -> UnaryParser {
    match op.as_str() {
        "-" => UnaryParser::Negate,
        "~" => UnaryParser::Complement,
        "!" => UnaryParser::Not,
        _ => unreachable!("Invalid Unary Operator in Parser: {}", op),
    }
}

fn expect(expected: &str, tokens: &mut Vec<String>) -> String {
    let actual = tokens.first().unwrap();
    if actual != expected {
        panic!("Found {}, expected {}", actual, expected);
    }
    tokens.remove(0)
}
