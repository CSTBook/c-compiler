use std::sync::Mutex;

use crate::parser::{self, BinaryParser, BlockItem, Function, Program, Statement, UnaryParser};

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
        body: parse_instructions(function_ast.body.first().unwrap()),
    }
}

fn parse_instructions(instructions_ast: &parser::BlockItem) -> Vec<TackyInstruction> {
    let mut instructions: Vec<TackyInstruction> = Vec::new();
    let instructions_ast = instructions_ast.clone();
    match instructions_ast {
        BlockItem::Declaration(_) => (),
        BlockItem::Statement(statement) => match statement {
            Statement::Return(exp) => {
                let temp = parse_exp(exp, &mut instructions);
                instructions.push(TackyInstruction::Return(temp));
            }
            Statement::Expression(expression) => todo!(),
            Statement::Null => todo!(),
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
        parser::Expression::Var(_) => todo!(),
        parser::Expression::Assignment(expression, expression1) => todo!(),
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
