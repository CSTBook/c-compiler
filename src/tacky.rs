use std::sync::Mutex;

use crate::parser::{
    self, BinaryParser, BlockItem, Expression, Function, Program, Statement, UnaryParser,
};

pub struct TackyProgram {
    pub function: TackyFunction,
}

pub struct TackyFunction {
    pub name: String,
    pub body: Vec<TackyInstruction>,
}

pub enum TackyInstruction {
    Return(TackyValue),
    Unary(UnaryParser, TackyValue, TackyValue), //Unary(unop, src, dest) where dest must be a temp var
    Binary(BinaryParser, TackyValue, TackyValue, TackyValue), //Binary(binop, src1, src2, dst)
    Copy(TackyValue, TackyValue),               //Copy(src, dst)
    Jump(String),                               //Jump(target)
    JumpIfZero(TackyValue, String),             //JumpIfZero(condition, target)
    JumpIfNotZero(TackyValue, String),          //JumpIfNotZero(condition, target)
    Label(String),                              //Label(identifier)
}

pub enum TackyValue {
    Constant(i32),
    Var(String),
}

static TEMP_COUNTER: Mutex<i32> = Mutex::new(0);

pub fn emit_tacky(program_ast: Program) -> TackyProgram {
    parse_program(program_ast)
}

fn parse_program(program_ast: Program) -> TackyProgram {
    TackyProgram {
        function: parse_function(program_ast.function),
    }
}

fn parse_function(function_ast: Function) -> TackyFunction {
    let name = function_ast.name;
    TackyFunction {
        name,
        body: {
            let mut temp: Vec<TackyInstruction> = Vec::new();
            for instruction in function_ast.body {
                temp.extend(parse_instructions(&instruction));
            }
            temp.push(TackyInstruction::Return(TackyValue::Constant(0))); //in case main function doesn't have a return statement
            temp
        },
    }
}

fn parse_instructions(instructions_ast: &parser::BlockItem) -> Vec<TackyInstruction> {
    let mut instructions: Vec<TackyInstruction> = Vec::new();
    let instructions_ast = instructions_ast.clone();
    match instructions_ast {
        BlockItem::Declaration(declaration) => {
            let (name, init) = (declaration.name, declaration.init);
            if let Some(exp) = init {
                let result = parse_exp(exp, &mut instructions);
                instructions.push(TackyInstruction::Copy(result, TackyValue::Var(name)));
            }
            // if the variable isnt initialized, we don't wanna do anything
        }
        BlockItem::Statement(statement) => match statement {
            Statement::Return(exp) => {
                let temp = parse_exp(exp, &mut instructions);
                instructions.push(TackyInstruction::Return(temp));
            }
            Statement::Expression(expression) => {
                let temp = parse_exp(expression, &mut instructions);
                instructions.push(TackyInstruction::Copy(
                    temp,
                    TackyValue::Var(make_temporary_variable()),
                ));
            }
            Statement::Null => (),
            Statement::If(cond, then, otherwise) => {
                let cond = parse_exp(cond, &mut instructions);
                let else_label = make_temporary_label("else");
                let end_label = make_temporary_label("end");

                if let Some(other) = otherwise {
                    instructions.push(TackyInstruction::JumpIfZero(cond, else_label.clone()));
                    instructions.extend(parse_instructions(&BlockItem::Statement(*then)));
                    instructions.push(TackyInstruction::Jump(end_label.clone()));
                    instructions.push(TackyInstruction::Label(else_label));
                    instructions.extend(parse_instructions(&BlockItem::Statement(*other)));
                } else {
                    instructions.push(TackyInstruction::JumpIfZero(cond, end_label.clone()));
                    instructions.extend(parse_instructions(&BlockItem::Statement(*then)));
                }

                instructions.push(TackyInstruction::Label(end_label));
            }
            Statement::Label(label_name) => instructions.push(TackyInstruction::Label(label_name)),
            Statement::Goto(label_name) => instructions.push(TackyInstruction::Jump(label_name)),
        },
    }
    instructions
}

fn parse_exp(
    expression: parser::Expression,
    instructions: &mut Vec<TackyInstruction>,
) -> TackyValue {
    match expression {
        parser::Expression::Constant(val) => TackyValue::Constant(val),
        parser::Expression::Unary(unop, exp) => {
            let src = parse_exp(*exp, instructions);
            let name = make_temporary_variable();
            let dest = TackyValue::Var(name.clone());
            instructions.push(TackyInstruction::Unary(
                unop,
                src,
                TackyValue::Var(name.clone()),
            ));

            dest
        }
        parser::Expression::Binary(binop, val1, val2) => match binop {
            BinaryParser::And => {
                let exp1 = parse_exp(*val1, instructions);
                let false_lab = make_temporary_label("and_false");
                instructions.push(TackyInstruction::JumpIfZero(exp1, false_lab.clone()));
                let exp2 = parse_exp(*val2, instructions);
                instructions.push(TackyInstruction::JumpIfZero(exp2, false_lab.clone()));
                let result = make_temporary_variable();
                instructions.push(TackyInstruction::Copy(
                    TackyValue::Constant(1),
                    TackyValue::Var(result.clone()),
                ));
                let end = make_temporary_label("end");
                instructions.push(TackyInstruction::Jump(end.clone()));
                instructions.push(TackyInstruction::Label(false_lab));
                instructions.push(TackyInstruction::Copy(
                    TackyValue::Constant(0),
                    TackyValue::Var(result.clone()),
                ));
                instructions.push(TackyInstruction::Label(end));

                TackyValue::Var(result)
            }
            BinaryParser::Or => {
                let exp1 = parse_exp(*val1.clone(), instructions);
                let true_lab = make_temporary_label("or_true");
                instructions.push(TackyInstruction::JumpIfNotZero(exp1, true_lab.clone()));
                let exp2 = parse_exp(*val2.clone(), instructions);
                instructions.push(TackyInstruction::JumpIfNotZero(exp2, true_lab.clone()));

                let result = make_temporary_variable(); //if not exp1 && not exp2
                instructions.push(TackyInstruction::Copy(
                    TackyValue::Constant(0),
                    TackyValue::Var(result.clone()),
                ));
                let end = make_temporary_label("end");
                instructions.push(TackyInstruction::Jump(end.clone()));

                instructions.push(TackyInstruction::Label(true_lab));
                instructions.push(TackyInstruction::Copy(
                    TackyValue::Constant(1),
                    TackyValue::Var(result.clone()),
                ));

                instructions.push(TackyInstruction::Label(end));
                TackyValue::Var(result)
            }
            BinaryParser::PostfixIncrement | BinaryParser::PostfixDecrement => {
                let tmp = make_temporary_variable();
                if let Expression::Var(name) = *val1 {
                    instructions.push(TackyInstruction::Copy(
                        TackyValue::Var(name.clone()),
                        TackyValue::Var(tmp.clone()),
                    ));
                    instructions.push(TackyInstruction::Binary(
                        match binop {
                            BinaryParser::PostfixIncrement => BinaryParser::Add,
                            BinaryParser::PostfixDecrement => BinaryParser::Subtract,
                            _ => unreachable!(),
                        },
                        TackyValue::Var(name.clone()),
                        TackyValue::Constant(1),
                        TackyValue::Var(name),
                    ));
                }

                TackyValue::Var(tmp)
            }
            _ => {
                let src1 = parse_exp(*val1, instructions);
                let src2 = parse_exp(*val2, instructions);
                let name = make_temporary_variable();
                let dst = TackyValue::Var(name.clone());
                instructions.push(TackyInstruction::Binary(
                    binop,
                    src1,
                    src2,
                    TackyValue::Var(name),
                ));

                dst
            }
        },
        parser::Expression::Var(name) => TackyValue::Var(name),
        parser::Expression::Assignment(var, exp) => {
            if let parser::Expression::Var(name) = *var {
                let result = parse_exp(*exp, instructions);
                instructions.push(TackyInstruction::Copy(
                    result,
                    TackyValue::Var(name.clone()),
                ));
                return TackyValue::Var(name);
            }
            unreachable!();
        }
        parser::Expression::Conditional(cond, then, right) => {
            let cond = parse_exp(*cond, instructions);
            let else_label = make_temporary_label("else");
            let end_label = make_temporary_label("end");
            let result = make_temporary_variable();

            instructions.push(TackyInstruction::JumpIfZero(cond, else_label.clone()));
            let val1 = parse_exp(*then, instructions);
            instructions.push(TackyInstruction::Copy(
                val1,
                TackyValue::Var(result.clone()),
            ));
            instructions.push(TackyInstruction::Jump(end_label.clone()));

            instructions.push(TackyInstruction::Label(else_label));
            let val2 = parse_exp(*right, instructions);
            instructions.push(TackyInstruction::Copy(
                val2,
                TackyValue::Var(result.clone()),
            ));

            instructions.push(TackyInstruction::Label(end_label));

            TackyValue::Var(result)
        }
    }
}

fn make_temporary_variable() -> String {
    let name = format!("tmp.{}", TEMP_COUNTER.lock().unwrap());
    *(TEMP_COUNTER.lock().unwrap()) += 1;
    name
}

fn make_temporary_label(label: &str) -> String {
    let name = format!("{}.{}", label, TEMP_COUNTER.lock().unwrap());
    *(TEMP_COUNTER.lock().unwrap()) += 1;
    name
}
