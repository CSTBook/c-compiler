use std::{collections::HashMap, sync::Mutex};

use crate::parser::*;

static TEMP_COUNTER: Mutex<i32> = Mutex::new(0);

struct MapEntry {
    name: String,
    from_curr_scope: bool,
}

pub fn semantic_analysis(ast_program: &mut Program) {
    variable_resolution(ast_program);
    let mut labels = label_resolution(ast_program);
    goto_resolution(&mut ast_program.function.body.body, &mut labels);
}

fn variable_resolution(ast_program: &mut Program) {
    let mut variable_map: HashMap<String, MapEntry> = HashMap::new();

    ast_program.function.body = resolve_block(&ast_program.function.body, &mut variable_map);
}

fn resolve_block(block: &Block, variable_map: &mut HashMap<String, MapEntry>) -> Block {
    let mut body = Vec::new();
    for block_item in block.body.clone() {
        body.push(match block_item {
            BlockItem::Statement(statement) => {
                BlockItem::Statement(resolve_statement(statement, variable_map))
            }
            BlockItem::Declaration(declaration) => {
                BlockItem::Declaration(resolve_declaration(declaration, variable_map))
            }
        });
    }

    Block { body }
}

fn label_resolution(ast_program: &mut Program) -> HashMap<String, String> {
    let mut labels: HashMap<String, String> = HashMap::new();

    ast_program.function.body = resolve_block_label(&ast_program.function.body, &mut labels, false);

    labels
}

fn resolve_block_label(
    block: &Block,
    labels: &mut HashMap<String, String>,
    check_goto: bool,
) -> Block {
    let mut body = Vec::new();

    //ensure no duplicate labels and assign the labeling scheme
    for block_item in block.body.clone() {
        body.push(match block_item {
            BlockItem::Statement(ref statement) => {
                BlockItem::Statement(resolve_statement_label(statement, labels, check_goto))
            }
            BlockItem::Declaration(_) => block_item,
        });
    }

    //(FOR C17) Ensure that a statement follows a label
    for i in 0..body.len() {
        if let BlockItem::Statement(Statement::Label(_)) = body.get(i).unwrap() {
            if let BlockItem::Statement(_) = body
                .get(i + 1)
                .expect("ERROR: Missing statement after label declaration")
            {
            } else {
                panic!("ERROR: Missing statement after label declaration");
            }
        }
    }

    Block { body }
}

fn goto_resolution(body: &mut Vec<BlockItem>, labels: &mut HashMap<String, String>) {
    //ensure that no goto statements to nonexistant labels
    for block_item in body {
        if let BlockItem::Statement(statement) = block_item {
            *block_item = BlockItem::Statement(resolve_statement_label(statement, labels, true));
        }
    }
}

fn resolve_statement_label(
    statement: &Statement,
    labels: &mut HashMap<String, String>,
    check_goto: bool,
) -> Statement {
    match statement {
        Statement::Label(label_name) => {
            if check_goto {
                //on goto_pass
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
                Box::new(resolve_statement_label(
                    otherwise_statement,
                    labels,
                    check_goto,
                ))
            }),
        ),
        Statement::Goto(label_name) => {
            if !check_goto {
                //not on goto_pass
                return statement.clone();
            }
            if !labels.contains_key(label_name) {
                panic!("ERROR: Undeclared label '{}'", label_name);
            }
            Statement::Goto(labels.get(label_name).unwrap().to_string())
        }
        Statement::Compound(block) => {
            Statement::Compound(resolve_block_label(block, labels, check_goto))
        }
        _ => statement.clone(),
    }
}

fn resolve_declaration(
    declaration: Declaration,
    variable_map: &mut HashMap<String, MapEntry>,
) -> Declaration {
    let (name, mut init) = (declaration.name, declaration.init);
    if variable_map.contains_key(&name) && variable_map.get(&name).unwrap().from_curr_scope {
        panic!("ERROR: Redeclared {name}");
    }
    let temp_name = make_temporary_variable(&name);
    variable_map.insert(
        name,
        MapEntry {
            name: temp_name.clone(),
            from_curr_scope: true,
        },
    );
    if let Some(exp) = init {
        init = Some(resolve_expression(exp, variable_map));
    }
    Declaration {
        name: temp_name,
        init,
    }
}

fn resolve_statement(
    statement: Statement,
    variable_map: &mut HashMap<String, MapEntry>,
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
        Statement::Compound(block) => {
            let mut new_var_map = copy_variable_map(variable_map);

            Statement::Compound(resolve_block(&block, &mut new_var_map))
        }
    }
}

fn resolve_expression(exp: Expression, variable_map: &mut HashMap<String, MapEntry>) -> Expression {
    match exp {
        Expression::Var(name) => {
            if variable_map.contains_key(&name) {
                return Expression::Var(variable_map.get(&name).unwrap().name.to_string());
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

fn copy_variable_map(variable_map: &HashMap<String, MapEntry>) -> HashMap<String, MapEntry> {
    let mut output: HashMap<String, MapEntry> = HashMap::new();

    for (name, map_entry) in variable_map {
        output.insert(
            name.clone(),
            MapEntry {
                name: map_entry.name.clone(),
                from_curr_scope: false,
            },
        );
    }

    output
}

//TODO: Add support for typedef
//Look at https://eli.thegreenplace.net/2011/05/02/the-context-sensitivity-of-cs-grammar-revisited
