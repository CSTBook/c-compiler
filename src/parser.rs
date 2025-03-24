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
    If(Expression, Box<Statement>, Option<Box<Statement>>),
    Label(String),
    Goto(String),
    Null,
}

#[derive(Clone)]
pub enum Expression {
    Constant(i32),
    Var(String),
    Unary(UnaryParser, Box<Expression>),
    Binary(BinaryParser, Box<Expression>, Box<Expression>),
    Assignment(Box<Expression>, Box<Expression>), // var = exp
    Conditional(Box<Expression>, Box<Expression>, Box<Expression>), //condition ? then : else
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
    PostfixIncrement,
    PostfixDecrement,
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
    let keywords = ["int", "return", "void", "goto", "if", "else"];
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
    let next_token = tokens.first().unwrap().as_str();
    if next_token == "return" {
        tokens.remove(0);
        let exp = parse_expression(tokens, 0);
        expect(";", tokens);
        Statement::Return(exp)
    } else if next_token == ";" {
        tokens.remove(0);
        Statement::Null
    } else if next_token == "if" {
        tokens.remove(0);
        expect("(", tokens);
        let condition = parse_expression(tokens, 0);
        expect(")", tokens);
        let then = parse_statement(tokens);
        let else_token = match tokens.first().unwrap().as_str() {
            "else" => {
                tokens.remove(0);
                Some(Box::new(parse_statement(tokens)))
            }
            _ => None,
        };

        Statement::If(condition, Box::new(then), else_token)
    } else if Regex::new(r"[a-zA-Z_]\w*\b:").unwrap().is_match(next_token) {
        //label
        let mut label_name = tokens.remove(0);
        label_name = label_name[..label_name.len() - 1].to_string();

        check_name(&label_name);

        Statement::Label(label_name)
    } else if next_token == "goto" {
        tokens.remove(0);
        let goto_label = tokens.remove(0);
        expect(";", tokens);

        Statement::Goto(goto_label)
    } else {
        let exp = parse_expression(tokens, 0);
        expect(";", tokens);
        Statement::Expression(exp)
    }
}

fn parse_expression(tokens: &mut Vec<String>, min_precedence: i32) -> Expression {
    let mut left = parse_factor(tokens);
    let mut next_token = tokens.first().unwrap().clone();
    let binary_tokens = [
        "+", "-", "*", "/", "%", "|", "%", "|", "^", "&", ">>", "<<", "&&", "||", "==", "!=", ">",
        "<", ">=", "<=", "=", "+=", "-=", "*=", "/=", "%=", "&=", "|=", "^=", "<<=", ">>=", "++",
        "?", "--",
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
            "+=" | "-=" | "*=" | "/=" | "%=" | "|=" | "&=" | "^=" | ">>=" | "<<=" => {
                let binop = parse_binop(tokens);
                let right = parse_expression(tokens, get_precedence(next_token.to_string())); //so that we get right associativity
                left = Expression::Assignment(
                    Box::new(left.clone()),
                    Box::new(Expression::Binary(binop, Box::new(left), Box::new(right))),
                );
            }
            "?" => {
                let middle = parse_conditional_middle(tokens);
                let right = parse_expression(tokens, get_precedence(next_token));
                left = Expression::Conditional(Box::new(left), Box::new(middle), Box::new(right));
            }
            "++" | "--" => {
                left = match &left {
                    Expression::Var(_) => Expression::Binary(
                        parse_binop(tokens),
                        Box::new(left),
                        Box::new(Expression::Constant(1)),
                    ),
                    Expression::Unary(unop, expression) => {
                        //since unary has lower precedence than postfix
                        match **expression {
                            Expression::Var(_) => Expression::Unary(
                                unop.clone(),
                                Box::new(Expression::Binary(
                                    parse_binop(tokens),
                                    expression.clone(),
                                    Box::new(Expression::Constant(1)),
                                )),
                            ),
                            _ => panic!("ERROR: Use of '{}' on non modifiable l-value", next_token),
                        }
                    }
                    _ => panic!("ERROR: Use of '{}' on non modifiable l-value", next_token),
                }
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

fn parse_conditional_middle(tokens: &mut Vec<String>) -> Expression {
    tokens.remove(0);
    let out = parse_expression(tokens, 0);
    tokens.remove(0);

    out
}

fn parse_factor(tokens: &mut Vec<String>) -> Expression {
    let mut next_token = tokens.first().unwrap().clone();
    if next_token.parse::<i32>().is_ok() {
        Expression::Constant(tokens.remove(0).parse::<i32>().unwrap())
    } else if next_token == "-" || next_token == "~" || next_token == "!" {
        let operator = parse_unop(tokens.remove(0));
        let inner_exp = parse_factor(tokens);
        Expression::Unary(operator, Box::new(inner_exp))
    } else if next_token == "--" || next_token == "++" {
        let binop = match tokens.remove(0).as_str() {
            "++" => BinaryParser::Add,
            "--" => BinaryParser::Subtract,
            _ => unreachable!(),
        };
        let exp = parse_factor(tokens);
        if !matches!(exp, Expression::Var(_)) {
            panic!("Attempted to use prefix on non-lvalue");
        }
        Expression::Assignment(
            Box::new(exp.clone()),
            Box::new(Expression::Binary(
                binop,
                Box::new(exp),
                Box::new(Expression::Constant(1)),
            )),
        )
    } else if next_token == "(" {
        tokens.remove(0);
        let inner_exp = parse_expression(tokens, 0);
        expect(")", tokens);
        inner_exp
    } else {
        if next_token.contains(":") {
            //deal with ternary ops
            next_token = next_token[0..next_token.len() - 1].to_string();
            tokens.insert(1, String::from(":"));
        }
        check_name(&next_token);
        Expression::Var(tokens.remove(0).replace(":", ""))
    }
}

fn get_precedence(token: String) -> i32 {
    match token.as_str() {
        "=" | "+=" | "-=" | "*=" | "/=" | "%=" | "&=" | "|=" | "^=" | ">>=" | "<<=" => 1,
        "?" => 2,
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
        "++" | "--" => 14,
        _ => unreachable!("Binary Operator Precedence Error"),
    }
}

fn parse_binop(tokens: &mut Vec<String>) -> BinaryParser {
    let binop = tokens.remove(0);
    match binop.as_str() {
        "+" | "+=" => BinaryParser::Add,
        "-" | "-=" => BinaryParser::Subtract,
        "*" | "*=" => BinaryParser::Multiply,
        "/" | "/=" => BinaryParser::Divide,
        "%" | "%=" => BinaryParser::Remainder,
        "|" | "|=" => BinaryParser::BitwiseOr,
        "^" | "^=" => BinaryParser::BitwiseXor,
        "&" | "&=" => BinaryParser::BitwiseAnd,
        "<<" | "<<=" => BinaryParser::BitwiseLeftShift,
        ">>" | ">>=" => BinaryParser::BitwiseRightShift,
        "&&" => BinaryParser::And,
        "||" => BinaryParser::Or,
        "==" => BinaryParser::Equal,
        "!=" => BinaryParser::NotEqual,
        ">" => BinaryParser::GreaterThan,
        ">=" => BinaryParser::GreaterOrEqual,
        "<" => BinaryParser::LessThan,
        "<=" => BinaryParser::LessOrEqual,
        "++" => BinaryParser::PostfixIncrement,
        "--" => BinaryParser::PostfixDecrement,
        _ => unreachable!("Binary Operator Parsing Error, '{}'", binop),
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