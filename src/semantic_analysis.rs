use std::{collections::HashMap, sync::Mutex};

use crate::parser::*;

static TEMP_COUNTER: Mutex<i32> = Mutex::new(0);

pub fn semantic_analysis(ast_program: &mut Program) {
    variable_resolution(ast_program);
    label_resolution(ast_program);
}

fn variable_resolution(ast_program: &mut Program) {
    let mut variable_map: HashMap<String, String> = HashMap::new();
    let mut body_clone = Vec::new();

    for block in ast_program.function.body.clone() {
        body_clone.push(match block {
            BlockItem::Statement(statement) => {
                BlockItem::Statement(resolve_statement(statement, &mut variable_map))
            }
            BlockItem::Declaration(declaration) => {
                BlockItem::Declaration(resolve_declaration(declaration, &mut variable_map))
            }
        });
    }

    ast_program.function.body = body_clone;
}

fn label_resolution(ast_program: &mut Program) {
    let mut labels: HashMap<String, String> = HashMap::new();
    let mut body_clone = Vec::new();
    
    //ensure no duplicate labels and assign the labeling scheme
    for block in ast_program.function.body.clone() {
        body_clone.push(match block {
            BlockItem::Statement(ref statement) => {
                BlockItem::Statement(resolve_statement_label(statement, &mut labels, false))
            }
            BlockItem::Declaration(_) => block,
        });
    }

    //ensure that no goto statements to nonexistant labels
    for block in &mut body_clone {
        if let BlockItem::Statement(statement) = block {
            *block = BlockItem::Statement(resolve_statement_label(statement, &mut labels, true));
        }
    }

    //(FOR C17) Ensure that a statement follows a label
    for i in 0..body_clone.len() {
        if let BlockItem::Statement(Statement::Label(_)) = body_clone.get(i).unwrap() {
            if let BlockItem::Statement(_) = body_clone.get(i+1).expect("ERROR: Missing statement after label declaration") {
            } else {
                panic!("ERROR: Missing statement after label declaration");
            }
        }
    }

    ast_program.function.body = body_clone;
}

fn resolve_statement_label(
    statement: &Statement,
    labels: &mut HashMap<String, String>,
    check_goto: bool,
) -> Statement {
    match statement {
        Statement::Label(label_name) => {
            if check_goto { //on goto_pass
                return statement.clone();
            }
            if labels.contains_key(label_name) {
                panic!("ERROR: Reused label name '{}'", label_name);
            }
            labels.insert(
                label_name.to_string(),
                make_temporary_label((label_name.to_string() + ".label").as_str()),
            );
            Statement::Label(labels.get(label_name).unwrap().to_string())
        }
        Statement::If(cond, then, otherwise) => Statement::If(
            cond.clone(),
            Box::new(resolve_statement_label(then, labels, check_goto)),
            otherwise.as_ref().map(|otherwise_statement| {
                Box::new(resolve_statement_label(otherwise_statement, labels, check_goto))
            }),
        ),
        Statement::Goto(label_name) => {
            if !check_goto { //not on goto_pass
                return statement.clone();
            }
            if !labels.contains_key(label_name) {
                panic!("ERROR: Undeclared label '{}'", label_name);
            }
            Statement::Goto(labels.get(label_name).unwrap().to_string())
        }
        _ => statement.clone(),
    }
}

fn resolve_declaration(
    declaration: Declaration,
    variable_map: &mut HashMap<String, String>,
) -> Declaration {
    let (name, mut init) = (declaration.name, declaration.init);
    if variable_map.contains_key(&name) {
        panic!("ERROR: Redeclared {name}");
    }
    let temp_name = make_temporary_variable(&name);
    variable_map.insert(name, temp_name.clone());
    if let Some(exp) = init {
        init = Some(resolve_expression(exp, variable_map));
    }
    Declaration {
        name: temp_name,
        init,
    }
}

fn make_temporary_variable(name: &str) -> String {
    let name = format!("{}.{}", name, TEMP_COUNTER.lock().unwrap());
    *(TEMP_COUNTER.lock().unwrap()) += 1;
    name
}

fn make_temporary_label(label: &str) -> String {
    let name = format!("{}.{}", label, TEMP_COUNTER.lock().unwrap());
    *(TEMP_COUNTER.lock().unwrap()) += 1;
    name
}

fn resolve_statement(
    statement: Statement,
    variable_map: &mut HashMap<String, String>,
) -> Statement {
    match statement {
        Statement::Return(exp) => Statement::Return(resolve_expression(exp, variable_map)),
        Statement::Expression(exp) => Statement::Expression(resolve_expression(exp, variable_map)),
        Statement::Null => Statement::Null,
        Statement::If(cond, then, otherwise) => Statement::If(
            resolve_expression(cond, variable_map),
            Box::new(resolve_statement(*then, variable_map)),
            otherwise.map(|stmt| Box::new(resolve_statement(*stmt, variable_map))),
        ),
        Statement::Label(_) => statement,
        Statement::Goto(_) => statement,
    }
}

fn resolve_expression(exp: Expression, variable_map: &mut HashMap<String, String>) -> Expression {
    match exp {
        Expression::Var(name) => {
            if variable_map.contains_key(&name) {
                return Expression::Var(variable_map.get(&name).unwrap().to_string());
            }
            panic!("ERROR: {} not found!", name);
        }
        Expression::Assignment(left, right) => {
            if let Expression::Var(_) = *left {
                return Expression::Assignment(
                    Box::new(resolve_expression(*left, variable_map)),
                    Box::new(resolve_expression(*right, variable_map)),
                );
            }
            panic!("ERROR: Improper lvalue!");
        }
        Expression::Constant(_) => exp,
        Expression::Unary(unop, exp) => {
            Expression::Unary(unop, Box::new(resolve_expression(*exp, variable_map)))
        }
        Expression::Binary(binop, exp1, exp2) => Expression::Binary(
            binop,
            Box::new(resolve_expression(*exp1, variable_map)),
            Box::new(resolve_expression(*exp2, variable_map)),
        ),
        Expression::Conditional(cond, middle, right) => Expression::Conditional(
            Box::new(resolve_expression(*cond, variable_map)),
            Box::new(resolve_expression(*middle, variable_map)),
            Box::new(resolve_expression(*right, variable_map)),
        ),
    }
}

//TODO: Add support for typedef
//Look at https://eli.thegreenplace.net/2011/05/02/the-context-sensitivity-of-cs-grammar-revisited
