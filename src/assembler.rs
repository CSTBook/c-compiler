use std::collections::HashMap;

use crate::{
    parser::{self, UnaryParser},
    tacky::{TackyFunction, TackyInstruction, TackyProgram, TackyValue},
};

pub struct AsmProgram {
    pub function: AsmFunction,
}

pub struct AsmFunction {
    pub name: String,
    pub instructions: Vec<Instruction>,
}

#[derive(Clone, PartialEq)]
pub enum UnaryOp {
    Neg,
    Not,
}
#[derive(Clone, PartialEq)]
pub enum Register {
    AX,
    CX,
    DX,
    R10,
    R11,
}

#[derive(Clone, PartialEq)]
pub enum Instruction {
    Mov(Operand, Operand),              //src, dst
    Unary(UnaryOp, Operand),            //unop, operand
    Binary(BinaryOp, Operand, Operand), //binop, src, dst (dst is the first operand)
    Cmp(Operand, Operand),
    Idiv(Operand),
    Cdq,
    Jmp(String),
    JmpCC(CondCode, String),
    SetCC(CondCode, Operand),
    Label(String),
    AllocateStack(i32),
    Ret,
}
#[derive(Clone, PartialEq)]
pub enum Operand {
    Imm(i32),       //val
    Reg(Register),  //reg
    Pseudo(String), //identifier
    Stack(i32),
}

#[derive(Clone, PartialEq)]
pub enum BinaryOp {
    Add,
    Sub,
    Mult,
    BitwiseAnd,
    BitwiseOr,
    BitwiseXor,
    BitwiseLeftShift,
    BitwiseRightShift,
}

#[derive(Clone, PartialEq)]
pub enum CondCode {
    E,
    NE,
    G,
    GE,
    L,
    LE,
}

pub fn assembler(program_ast: TackyProgram) -> AsmProgram {
    let mut program = parse_program(program_ast);
    let offset = replace_pseudoregister(&mut program);
    fix_instructions(offset, &mut program);

    program
}

fn parse_program(program_ast: TackyProgram) -> AsmProgram {
    AsmProgram {
        function: parse_function(program_ast.function),
    }
}

fn parse_function(function_ast: TackyFunction) -> AsmFunction {
    AsmFunction {
        name: function_ast.name,
        instructions: parse_instruction(function_ast.body),
    }
}

fn parse_instruction(tacky_instructions: Vec<TackyInstruction>) -> Vec<Instruction> {
    let mut instructions: Vec<Instruction> = Vec::new();

    for instruction in tacky_instructions {
        match instruction {
            TackyInstruction::Return(val) => {
                instructions.push(Instruction::Mov(
                    parse_operand(&val),
                    Operand::Reg(Register::AX),
                ));
                instructions.push(Instruction::Ret);
            }
            TackyInstruction::Unary(unop, src, dest) => match unop {
                UnaryParser::Not => {
                    instructions.push(Instruction::Cmp(Operand::Imm(0), parse_operand(&src)));
                    instructions.push(Instruction::Mov(Operand::Imm(0), parse_operand(&dest)));
                    instructions.push(Instruction::SetCC(CondCode::E, parse_operand(&dest)));
                }
                _ => {
                    instructions.push(Instruction::Mov(parse_operand(&src), parse_operand(&dest)));
                    instructions.push(Instruction::Unary(parse_unop(unop), parse_operand(&dest)));
                }
            },
            TackyInstruction::Binary(binop, src1, src2, dst) => match binop {
                parser::BinaryParser::Add
                | parser::BinaryParser::Multiply
                | parser::BinaryParser::Subtract
                | parser::BinaryParser::BitwiseAnd
                | parser::BinaryParser::BitwiseOr
                | parser::BinaryParser::BitwiseXor
                | parser::BinaryParser::BitwiseLeftShift
                | parser::BinaryParser::BitwiseRightShift => {
                    instructions.push(Instruction::Mov(parse_operand(&src1), parse_operand(&dst)));
                    instructions.push(Instruction::Binary(
                        parse_binop(binop),
                        parse_operand(&src2),
                        parse_operand(&dst),
                    ));
                }
                parser::BinaryParser::Divide => {
                    instructions.push(Instruction::Mov(
                        parse_operand(&src1),
                        Operand::Reg(Register::AX),
                    ));
                    instructions.push(Instruction::Cdq);
                    instructions.push(Instruction::Idiv(parse_operand(&src2)));
                    instructions.push(Instruction::Mov(
                        Operand::Reg(Register::AX),
                        parse_operand(&dst),
                    ));
                }
                parser::BinaryParser::Remainder => {
                    instructions.push(Instruction::Mov(
                        parse_operand(&src1),
                        Operand::Reg(Register::AX),
                    ));
                    instructions.push(Instruction::Cdq);
                    instructions.push(Instruction::Idiv(parse_operand(&src2)));
                    instructions.push(Instruction::Mov(
                        Operand::Reg(Register::DX),
                        parse_operand(&dst),
                    ));
                }
                parser::BinaryParser::Equal => {
                    instructions.push(Instruction::Cmp(parse_operand(&src2), parse_operand(&src1)));
                    instructions.push(Instruction::Mov(Operand::Imm(0), parse_operand(&dst)));
                    instructions.push(Instruction::SetCC(CondCode::E, parse_operand(&dst)));
                }
                parser::BinaryParser::NotEqual => {
                    instructions.push(Instruction::Cmp(parse_operand(&src2), parse_operand(&src1)));
                    instructions.push(Instruction::Mov(Operand::Imm(0), parse_operand(&dst)));
                    instructions.push(Instruction::SetCC(CondCode::NE, parse_operand(&dst)));
                }
                parser::BinaryParser::LessThan => {
                    instructions.push(Instruction::Cmp(parse_operand(&src2), parse_operand(&src1)));
                    instructions.push(Instruction::Mov(Operand::Imm(0), parse_operand(&dst)));
                    instructions.push(Instruction::SetCC(CondCode::L, parse_operand(&dst)));
                }
                parser::BinaryParser::LessOrEqual => {
                    instructions.push(Instruction::Cmp(parse_operand(&src2), parse_operand(&src1)));
                    instructions.push(Instruction::Mov(Operand::Imm(0), parse_operand(&dst)));
                    instructions.push(Instruction::SetCC(CondCode::LE, parse_operand(&dst)));
                }
                parser::BinaryParser::GreaterThan => {
                    instructions.push(Instruction::Cmp(parse_operand(&src2), parse_operand(&src1)));
                    instructions.push(Instruction::Mov(Operand::Imm(0), parse_operand(&dst)));
                    instructions.push(Instruction::SetCC(CondCode::G, parse_operand(&dst)));
                }
                parser::BinaryParser::GreaterOrEqual => {
                    instructions.push(Instruction::Cmp(parse_operand(&src2), parse_operand(&src1)));
                    instructions.push(Instruction::Mov(Operand::Imm(0), parse_operand(&dst)));
                    instructions.push(Instruction::SetCC(CondCode::GE, parse_operand(&dst)));
                }
                _ => (),
            },
            TackyInstruction::Copy(src, dst) => {
                instructions.push(Instruction::Mov(parse_operand(&src), parse_operand(&dst)))
            }
            TackyInstruction::Jump(target) => instructions.push(Instruction::Jmp(target)),
            TackyInstruction::JumpIfZero(condition, target) => {
                instructions.push(Instruction::Cmp(Operand::Imm(0), parse_operand(&condition)));
                instructions.push(Instruction::JmpCC(CondCode::E, target));
            }
            TackyInstruction::JumpIfNotZero(condition, target) => {
                instructions.push(Instruction::Cmp(Operand::Imm(0), parse_operand(&condition)));
                instructions.push(Instruction::JmpCC(CondCode::NE, target));
            }
            TackyInstruction::Label(label) => instructions.push(Instruction::Label(label)),
        }
    }

    instructions
}

fn parse_operand(op_ast: &TackyValue) -> Operand {
    match op_ast {
        TackyValue::Constant(val) => Operand::Imm(*val),
        TackyValue::Var(name) => Operand::Pseudo(name.clone()),
    }
}

fn parse_unop(unop: parser::UnaryParser) -> UnaryOp {
    match unop {
        parser::UnaryParser::Complement => UnaryOp::Not,
        parser::UnaryParser::Negate => UnaryOp::Neg,
        parser::UnaryParser::Not => unreachable!("Should have handled Not Unary Op in Assembler"),
    }
}

fn parse_binop(binop: parser::BinaryParser) -> BinaryOp {
    match binop {
        parser::BinaryParser::Add => BinaryOp::Add,
        parser::BinaryParser::Subtract => BinaryOp::Sub,
        parser::BinaryParser::Multiply => BinaryOp::Mult,
        parser::BinaryParser::BitwiseAnd => BinaryOp::BitwiseAnd,
        parser::BinaryParser::BitwiseOr => BinaryOp::BitwiseOr,
        parser::BinaryParser::BitwiseXor => BinaryOp::BitwiseXor,
        parser::BinaryParser::BitwiseLeftShift => BinaryOp::BitwiseLeftShift,
        parser::BinaryParser::BitwiseRightShift => BinaryOp::BitwiseRightShift,
        _ => unreachable!("Binop Assembler Error"),
    }
}
//replaces variables with stack addresses
fn replace_pseudoregister(program: &mut AsmProgram) -> i32 {
    let mut temp_vars: HashMap<String, i32> = HashMap::new();

    for instruction in &mut program.function.instructions {
        *instruction = match instruction {
            Instruction::Mov(src, dst) => {
                let new_src = replace_operand(src, &mut temp_vars);
                let new_dst = replace_operand(dst, &mut temp_vars);

                Instruction::Mov(new_src, new_dst)
            }
            Instruction::Unary(unop, operand) => {
                let new_op = replace_operand(operand, &mut temp_vars);

                Instruction::Unary(unop.clone(), new_op)
            }
            Instruction::Binary(binop, src, dst) => {
                let new_src = replace_operand(src, &mut temp_vars);
                let new_dst = replace_operand(dst, &mut temp_vars);

                Instruction::Binary(binop.clone(), new_src, new_dst)
            }
            Instruction::Idiv(src) => {
                let new_src = replace_operand(src, &mut temp_vars);

                Instruction::Idiv(new_src)
            }
            Instruction::Cmp(op1, op2) => {
                let new_op1 = replace_operand(op1, &mut temp_vars);
                let new_op2 = replace_operand(op2, &mut temp_vars);

                Instruction::Cmp(new_op1, new_op2)
            }
            Instruction::SetCC(cc, dst) => {
                let new_dst = replace_operand(dst, &mut temp_vars);

                Instruction::SetCC(cc.clone(), new_dst)
            }
            _ => instruction.clone(),
        };
    }

    // program.function.instructions = instructions;

    4 * (temp_vars.len() as i32)
}

fn replace_operand(operand: &Operand, temp_vars: &mut HashMap<String, i32>) -> Operand {
    if let Operand::Pseudo(name) = operand {
        if !temp_vars.contains_key(name) {
            temp_vars.insert(name.clone(), -4 * (temp_vars.len() as i32 + 1));
        }
        Operand::Stack(*temp_vars.get(name).unwrap())
    } else {
        operand.clone()
    }
}

//fix improper operands
fn fix_instructions(offset: i32, program: &mut AsmProgram) {
    program
        .function
        .instructions
        .insert(0, Instruction::AllocateStack(offset));
    let mut instructions_clone = program.function.instructions.clone();
    for index in (1..instructions_clone.len()).rev() {
        match instructions_clone.clone().get(index).unwrap() {
            Instruction::Mov(src, dst) => {
                if let (Operand::Stack(_), Operand::Stack(_)) = (src, dst) {
                    //both stack ops
                    instructions_clone.remove(index);
                    instructions_clone.insert(
                        index,
                        Instruction::Mov(src.clone(), Operand::Reg(Register::R10)),
                    );
                    instructions_clone.insert(
                        index + 1,
                        Instruction::Mov(Operand::Reg(Register::R10), dst.clone()),
                    );
                }
            }
            Instruction::Binary(binop, src, dst) => match binop {
                BinaryOp::Add | BinaryOp::Sub => {
                    if let (Operand::Stack(_), Operand::Stack(_)) = (src, dst) {
                        //both stack ops
                        instructions_clone.remove(index);
                        instructions_clone.insert(
                            index,
                            Instruction::Mov(src.clone(), Operand::Reg(Register::R10)),
                        );
                        instructions_clone.insert(
                            index + 1,
                            Instruction::Binary(
                                binop.clone(),
                                Operand::Reg(Register::R10),
                                dst.clone(),
                            ),
                        );
                    }
                }
                BinaryOp::Mult => {
                    if let Operand::Stack(_) = dst {
                        instructions_clone.remove(index);
                        //use R11 since we are moving the destination rather than the source
                        instructions_clone.insert(
                            index,
                            Instruction::Mov(dst.clone(), Operand::Reg(Register::R11)),
                        );
                        instructions_clone.insert(
                            index + 1,
                            Instruction::Binary(
                                binop.clone(),
                                src.clone(),
                                Operand::Reg(Register::R11),
                            ),
                        );
                        instructions_clone.insert(
                            index + 2,
                            Instruction::Mov(Operand::Reg(Register::R11), dst.clone()),
                        );
                    }
                }
                BinaryOp::BitwiseAnd | BinaryOp::BitwiseOr | BinaryOp::BitwiseXor => {
                    //dest can be register or stack
                    if let Operand::Imm(val) = dst {
                        instructions_clone.remove(index);
                        instructions_clone.insert(
                            index,
                            Instruction::Mov(Operand::Imm(*val), Operand::Reg(Register::R11)),
                        );
                        instructions_clone.insert(
                            index + 1,
                            Instruction::Binary(
                                binop.clone(),
                                src.clone(),
                                Operand::Reg(Register::R11),
                            ),
                        );
                    }
                    if let (Operand::Stack(_), Operand::Stack(_)) = (src, dst) {
                        instructions_clone.remove(index);
                        instructions_clone.insert(
                            index,
                            Instruction::Mov(src.clone(), Operand::Reg(Register::R10)),
                        );
                        instructions_clone.insert(
                            index + 1,
                            Instruction::Binary(
                                binop.clone(),
                                Operand::Reg(Register::R10),
                                dst.clone(),
                            ),
                        );
                    }
                }
                BinaryOp::BitwiseRightShift | BinaryOp::BitwiseLeftShift => {
                    //src(count) is an imm or CL register, dst is register or stack
                    match src {
                        Operand::Stack(_) | Operand::Reg(_) => {
                            instructions_clone.remove(index);
                            //use R11 since we are moving the destination rather than the source
                            instructions_clone.insert(
                                index,
                                Instruction::Mov(src.clone(), Operand::Reg(Register::CX)),
                            );
                            instructions_clone.insert(
                                index + 1,
                                Instruction::Binary(
                                    binop.clone(),
                                    Operand::Reg(Register::CX),
                                    dst.clone(),
                                ),
                            );
                        }
                        _ => (),
                    }
                    if let Operand::Imm(_) = dst {
                        instructions_clone.remove(index);
                        instructions_clone.insert(
                            index,
                            Instruction::Binary(
                                binop.clone(),
                                src.clone(),
                                Operand::Reg(Register::R10),
                            ),
                        );
                        instructions_clone.insert(
                            index + 1,
                            Instruction::Mov(Operand::Reg(Register::R10), dst.clone()),
                        );
                    }
                }
            },
            Instruction::Idiv(Operand::Imm(val)) => {
                //moves imm operand into register and passes that as arg
                instructions_clone.remove(index);
                instructions_clone.insert(
                    index,
                    Instruction::Mov(Operand::Imm(*val), Operand::Reg(Register::R10)),
                );
                instructions_clone
                    .insert(index + 1, Instruction::Idiv(Operand::Reg(Register::R10)));
            }
            Instruction::Cmp(op1, op2) => {
                if let (Operand::Stack(_), Operand::Stack(_)) = (op1, op2) {
                    instructions_clone.remove(index);
                    instructions_clone.insert(
                        index,
                        Instruction::Mov(op1.clone(), Operand::Reg(Register::R10)),
                    );
                    instructions_clone.insert(
                        index + 1,
                        Instruction::Cmp(Operand::Reg(Register::R10), op2.clone()),
                    );
                }
                if let Operand::Imm(_) = op2 {
                    instructions_clone.remove(index);
                    instructions_clone.insert(
                        index,
                        Instruction::Mov(op2.clone(), Operand::Reg(Register::R11)),
                    );
                    instructions_clone.insert(
                        index + 1,
                        Instruction::Cmp(op1.clone(), Operand::Reg(Register::R11)),
                    );
                }
            }
            _ => (),
        }
    }

    program.function.instructions = instructions_clone;
}
