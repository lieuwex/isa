use instruction::*;
use opcode::*;

pub fn convert_instruction(instr: &InternalInstruction) -> Vec<Instruction> {
    let immediate = match instr.immediate{
        Immediate::Value(imm) => imm,
        _ => panic!("invalid instruction immediate"),
    };

    match instr.opcode {
        InternalOpcode::Opaque(OpaqueOpcode::j) => {
            vec![Instruction {
                opcode: Opcode::jnz,
                rs1: 0,
                rs2: 0,
                rd: 0,
                immediate: immediate,
            }]
        }

        InternalOpcode::Opaque(OpaqueOpcode::gt) => {
            vec![Instruction {
                opcode: Opcode::lt,
                rs1: instr.rs2,
                rs2: instr.rs1,
                rd: instr.rd,
                immediate: 0,
            }]
        }
        InternalOpcode::Opaque(OpaqueOpcode::gte) => {
            vec![Instruction {
                opcode: Opcode::lte,
                rs1: instr.rs2,
                rs2: instr.rs1,
                rd: instr.rd,
                immediate: 0,
            }]
        }

        InternalOpcode::Real(opcode) => {
            vec![Instruction {
                opcode: opcode,
                rs1: instr.rs1,
                rs2: instr.rs2,
                rd: instr.rd,
                immediate: immediate,
            }]
        }
    }
}
