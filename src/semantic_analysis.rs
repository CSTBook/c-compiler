use std::{collections::HashMap, sync::Mutex};

use crate::parser::*;

static TEMP_COUNTER: Mutex<i32> = Mutex::new(0);

pub fn semantic_analysis(ast_program: &mut Program) {
    variable_resolution(ast_program);
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

fn resolve_statement(
    statement: Statement,
    variable_map: &mut HashMap<String, String>,
) -> Statement {
    match statement {
        Statement::Return(exp) => Statement::Return(resolve_expression(exp, variable_map)),
        Statement::Expression(exp) => Statement::Expression(resolve_expression(exp, variable_map)),
        Statement::Null => Statement::Null,
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
    }
}

//TODO: Add support for typedef
//Look at https://eli.thegreenplace.net/2011/05/02/the-context-sensitivity-of-cs-grammar-revisited
