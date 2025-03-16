use std::collections::HashMap;

use crate::{
    parser::{self},
    tacky::{TackyInstruction, TackyNode, TackyValue},
};

pub enum AsmNode {
    Program(Box<AsmNode>),              //func
    Function(String, Vec<Instruction>), //name, instructions
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
    CL,
    DX,
    R10,
    R11,
}

#[derive(Clone, PartialEq)]
pub enum Instruction {
    Mov(Operand, Operand),              //src, dst
    Unary(UnaryOp, Operand),            //unop, operand
    Binary(BinaryOp, Operand, Operand), //binop, src, dst (dst is the first operand)
    Idiv(Operand),
    Cdq,
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

pub fn assembler(program_ast: TackyNode) -> AsmNode {
    let mut program = parse_program(program_ast);
    let offset = replace_pseudoregister(&mut program);
    fix_instructions(offset, &mut program);

    program
}

fn parse_program(program_ast: TackyNode) -> AsmNode {
    match program_ast {
        TackyNode::Program(func) => AsmNode::Program(Box::new(parse_function(*func))),
        _ => panic!("Tacky Program Assembler Error"),
    }
}

fn parse_function(function_ast: TackyNode) -> AsmNode {
    match function_ast {
        TackyNode::Function(name, instructions) => {
            AsmNode::Function(name, parse_instruction(instructions))
        }
        _ => panic!("Tacky Function Assembler Error"),
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
            TackyInstruction::Unary(unop, src, dest) => {
                instructions.push(Instruction::Mov(parse_operand(&src), parse_operand(&dest)));
                instructions.push(Instruction::Unary(parse_unop(unop), parse_operand(&dest)));
            }
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
                parser::BinaryParser::And => todo!(),
                parser::BinaryParser::Or => todo!(),
                parser::BinaryParser::Equal => todo!(),
                parser::BinaryParser::NotEqual => todo!(),
                parser::BinaryParser::LessThan => todo!(),
                parser::BinaryParser::LessOrEqual => todo!(),
                parser::BinaryParser::GreaterThan => todo!(),
                parser::BinaryParser::GreaterOrEqual => todo!(),
            },
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
        parser::UnaryParser::Not => todo!(),
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
fn replace_pseudoregister(program: &mut AsmNode) -> i32 {
    let mut temp_vars: HashMap<String, i32> = HashMap::new();

    if let AsmNode::Program(func) = program {
        if let AsmNode::Function(_, ref mut instructions) = **func {
            for instruction in instructions.iter_mut() {
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
                    _ => instruction.clone(),
                };
            }
        }
    }

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

//replaces mov, add, sub, mul addresses where both operands are addresses with address and register
//also replaces operand of idiv with register if immediate
fn fix_instructions(offset: i32, program: &mut AsmNode) {
    if let AsmNode::Program(func) = program {
        if let AsmNode::Function(_, ref mut instructions) = **func {
            instructions.insert(0, Instruction::AllocateStack(offset));
            let mut instructions_clone = instructions.clone();
            for index in (1..instructions_clone.len()).rev() {
                match instructions_clone.get(index).unwrap() {
                    Instruction::Mov(src, dst) => {
                        if let (Operand::Stack(_), Operand::Stack(_)) = (src, dst) {
                            //both stack ops
                            instructions.remove(index);
                            instructions.insert(
                                index,
                                Instruction::Mov(src.clone(), Operand::Reg(Register::R10)),
                            );
                            instructions.insert(
                                index + 1,
                                Instruction::Mov(Operand::Reg(Register::R10), dst.clone()),
                            );
                            instructions_clone = instructions.clone();
                        }
                    }
                    Instruction::Binary(binop, src, dst) => match binop {
                        BinaryOp::Add | BinaryOp::Sub => {
                            if let (Operand::Stack(_), Operand::Stack(_)) = (src, dst) {
                                //both stack ops
                                instructions.remove(index);
                                instructions.insert(
                                    index,
                                    Instruction::Mov(src.clone(), Operand::Reg(Register::R10)),
                                );
                                instructions.insert(
                                    index + 1,
                                    Instruction::Binary(
                                        binop.clone(),
                                        Operand::Reg(Register::R10),
                                        dst.clone(),
                                    ),
                                );
                                instructions_clone = instructions.clone();
                            }
                        }
                        BinaryOp::Mult => {
                            if let Operand::Stack(_) = dst {
                                instructions.remove(index);
                                //use R11 since we are moving the destination rather than the source
                                instructions.insert(
                                    index,
                                    Instruction::Mov(dst.clone(), Operand::Reg(Register::R11)),
                                );
                                instructions.insert(
                                    index + 1,
                                    Instruction::Binary(
                                        binop.clone(),
                                        src.clone(),
                                        Operand::Reg(Register::R11),
                                    ),
                                );
                                instructions.insert(
                                    index + 2,
                                    Instruction::Mov(Operand::Reg(Register::R11), dst.clone()),
                                );
                                instructions_clone = instructions.clone();
                            }
                        }
                        BinaryOp::BitwiseAnd | BinaryOp::BitwiseOr | BinaryOp::BitwiseXor => {
                            //dest can be register or stack
                            if let Operand::Imm(val) = dst {
                                instructions.remove(index);
                                instructions.insert(
                                    index,
                                    Instruction::Mov(
                                        Operand::Imm(*val),
                                        Operand::Reg(Register::R11),
                                    ),
                                );
                                instructions.insert(
                                    index + 1,
                                    Instruction::Binary(
                                        binop.clone(),
                                        src.clone(),
                                        Operand::Reg(Register::R11),
                                    ),
                                );
                            }
                            if let (Operand::Stack(_), Operand::Stack(_)) = (src, dst) {
                                instructions.remove(index);
                                instructions.insert(
                                    index,
                                    Instruction::Mov(src.clone(), Operand::Reg(Register::R10)),
                                );
                                instructions.insert(
                                    index + 1,
                                    Instruction::Binary(
                                        binop.clone(),
                                        Operand::Reg(Register::R10),
                                        dst.clone(),
                                    ),
                                );
                            }
                            instructions_clone = instructions.clone();
                        }
                        BinaryOp::BitwiseRightShift | BinaryOp::BitwiseLeftShift => {
                            //src(count) is an imm or CL register, dst is register or stack
                            match src {
                                Operand::Stack(_) | Operand::Reg(_) => {
                                    instructions.remove(index);
                                    //use R11 since we are moving the destination rather than the source
                                    instructions.insert(
                                        index,
                                        Instruction::Mov(src.clone(), Operand::Reg(Register::CX)),
                                    );
                                    instructions.insert(
                                        index + 1,
                                        Instruction::Binary(
                                            binop.clone(),
                                            Operand::Reg(Register::CL),
                                            dst.clone(),
                                        ),
                                    );
                                }
                                _ => (),
                            }
                            if let Operand::Imm(_) = dst {
                                instructions.remove(index);
                                instructions.insert(
                                    index,
                                    Instruction::Binary(
                                        binop.clone(),
                                        src.clone(),
                                        Operand::Reg(Register::R10),
                                    ),
                                );
                                instructions.insert(
                                    index + 1,
                                    Instruction::Mov(Operand::Reg(Register::R10), dst.clone()),
                                );
                            }
                            instructions_clone = instructions.clone();
                        }
                    },
                    Instruction::Idiv(Operand::Imm(val)) => {
                        //moves imm operand into register and passes that as arg
                        instructions.remove(index);
                        instructions.insert(
                            index,
                            Instruction::Mov(Operand::Imm(*val), Operand::Reg(Register::R10)),
                        );
                        instructions
                            .insert(index + 1, Instruction::Idiv(Operand::Reg(Register::R10)));
                        instructions_clone = instructions.clone();
                    }
                    _ => (),
                }
            }
        }
    }
}
