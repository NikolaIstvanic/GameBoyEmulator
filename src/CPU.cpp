/**
 * CPU.cpp: class which programmatically recreates the GameBoy's Zilog z80 CPU
 */
#include <iomanip>
#include <iostream>

#include "GameBoy.hpp"
#include "CPU.hpp"

CPU::CPU() {
    instruction_rom = {{
        {"NOP", &CPU::NOP, 4}, {"LD BC,nn", &CPU::LDBCnn, 12}, {"LD (BC),A", &CPU::LDmBCA, 8},
        {"INC BC", &CPU::INCBC, 8}, {"INC B", &CPU::INCB, 4}, {"DEC B", &CPU::DECB, 4},
        {"LD B,n", &CPU::LDBn, 8}, {"RLCA", &CPU::RLCA, 4}, {"LD (nn), SP", &CPU::LDmnnSP, 20},
        {"ADD HL,BC", &CPU::ADDHLBC, 8}, {"LD A,(BC)", &CPU::LDAmBC, 8}, {"DEC BC", &CPU::DECBC, 8},
        {"INC C", &CPU::INCC, 4}, {"DEC C", &CPU::DECC, 4}, {"LD C,n", &CPU::LDCn, 8},
        {"RRCA", &CPU::RRCA, 4}, {"STOP", &CPU::STOP, 4}, {"LD DE,nn", &CPU::LDDEnn, 12},
        {"LD (DE),A", &CPU::LDmDEA, 8}, {"INC DE", &CPU::INCDE,  8}, {"INC D", &CPU::INCD, 4},
        {"DEC D", &CPU::DECD, 4}, {"LD D,n", &CPU::LDDn, 8}, {"RLA", &CPU::RLA, 4},
        {"JR n", &CPU::JRn, 12}, {"ADD HL,DE", &CPU::ADDHLDE, 8}, {"LD A,(DE)", &CPU::LDAmDE, 8},
        {"DEC DE", &CPU::DECDE, 8}, {"INC E", &CPU::INCE, 4}, {"DEC E", &CPU::DECE, 4},
        {"LD E,n", &CPU::LDEn, 8}, {"RRA", &CPU::RRA, 4}, {"JRnz,n", &CPU::JRnzn, 8},
        {"LD HL,nn", &CPU::LDHLnn, 12}, {"LD (HL+),A", &CPU::LDmHLpA, 8},
        {"INC HL", &CPU::INCHL, 8}, {"INC H", &CPU::INCH, 4}, {"DEC H", &CPU::DECH, 4},
        {"LD H,n", &CPU::LDHn, 8}, {"DAA", &CPU::DAA, 4}, {"JRz,n", &CPU::JRzn, 8},
        {"ADD HL,HL", &CPU::ADDHLHL, 8}, {"LD A,(HL+)", &CPU::LDAmHLp, 8},
        {"DEC HL", &CPU::DECHL, 8}, {"INC L", &CPU::INCL, 4}, {"DEC L", &CPU::DECL, 4},
        {"LD L,n", &CPU::LDLn, 8}, {"CPL", &CPU::CPL, 4},
        {"JRnc,n", &CPU::JRncn, 8}, {"LD SP,nn", &CPU::LDSPnn, 12},
        {"LD (HL-),A", &CPU::LDmHLmA, 8}, {"INC SP", &CPU::INCSP, 8},
        {"INC (HL)", &CPU::INCmHL, 12}, {"DEC (HL)", &CPU::DECmHL, 12},
        {"LD (HL),n", &CPU::LDmHLn, 12}, {"SCF", &CPU::SCF, 4}, {"JRc,n", &CPU::JRcn, 8},
        {"ADD HL,SP", &CPU::ADDHLSP, 8}, {"LD A,(HL-)", &CPU::LDAmHLm, 8},
        {"DEC SP", &CPU::DECSP, 8}, {"INC A", &CPU::INCA, 4}, {"DEC A", &CPU::DECA, 4},
        {"LD A,n", &CPU::LDAn, 8}, {"CCF", &CPU::CCF, 4}, {"LD B,B", &CPU::LDBB, 4},
        {"LD B,C", &CPU::LDBC, 4}, {"LD B,D", &CPU::LDBD, 4}, {"LD B,E", &CPU::LDBE, 4},
        {"LD B,H", &CPU::LDBH, 4}, {"LD B,L", &CPU::LDBL, 4}, {"LD B,(HL)", &CPU::LDBmHL, 8},
        {"LD B,A", &CPU::LDBA, 4}, {"LD C,B", &CPU::LDCB, 4}, {"LD C,C", &CPU::LDCC, 4},
        {"LD C,D", &CPU::LDCD, 4}, {"LD C,E", &CPU::LDCE, 4}, {"LD C,H", &CPU::LDCH, 4},
        {"LD C,L", &CPU::LDCL, 4}, {"LD C,(HL)", &CPU::LDCmHL, 8}, {"LD C,A", &CPU::LDCA, 4},
        {"LD D,B", &CPU::LDDB, 4}, {"LD D,C", &CPU::LDDC, 4}, {"LD D,D", &CPU::LDDD, 4},
        {"LD D,E", &CPU::LDDE, 4}, {"LD D,H", &CPU::LDDH, 4}, {"LD D,L", &CPU::LDDL, 4},
        {"LD D,(HL)", &CPU::LDDmHL, 8}, {"LD D,A", &CPU::LDDA, 4}, {"LD E,B", &CPU::LDEB, 4},
        {"LD E,C", &CPU::LDEC, 4}, {"LD E,D", &CPU::LDED, 4}, {"LD E,E", &CPU::LDEE, 4},
        {"LD E,H", &CPU::LDEH, 4}, {"LD E,L", &CPU::LDEL, 4}, {"LD E,(HL)", &CPU::LDEmHL, 8},
        {"LD E,A", &CPU::LDEA, 4}, {"LD H,B", &CPU::LDHB, 4}, {"LD H,C", &CPU::LDHC, 4},
        {"LD H,D", &CPU::LDHD, 4}, {"LD H,E", &CPU::LDHE, 4}, {"LD H,H", &CPU::LDHH, 4},
        {"LD H,L", &CPU::LDHL, 4}, {"LD H,(HL)", &CPU::LDHmHL, 8}, {"LD H,A", &CPU::LDHA, 4},
        {"LD L,B", &CPU::LDLB, 4}, {"LD L,C", &CPU::LDLC, 4}, {"LD L,D", &CPU::LDLD, 4}, 
        {"LD L,E", &CPU::LDLE, 4},  {"LD L,H", &CPU::LDLH, 4}, {"LD L,L", &CPU::LDLL, 4},
        {"LD L,(HL)", &CPU::LDLmHL, 8}, {"LD L,A", &CPU::LDLA, 4}, {"LD (HL),B", &CPU::LDmHLB, 8},
        {"LD (HL),C", &CPU::LDmHLC, 8}, {"LD (HL),D", &CPU::LDmHLD, 8},
        {"LD (HL),E", &CPU::LDmHLE, 8}, {"LD (HL),H", &CPU::LDmHLH, 8},
        {"LD (HL),L", &CPU::LDmHLL, 8}, {"HALT", &CPU::HALT, 4}, {"LD (HL),A", &CPU::LDmHLA, 8},
        {"LD A,B", &CPU::LDAB, 4}, {"LD A,C", &CPU::LDAC, 4}, {"LD A,D", &CPU::LDAD, 4},
        {"LD A,E", &CPU::LDAE, 4}, {"LD A,H", &CPU::LDAH, 4}, {"LD A,L", &CPU::LDAL, 4},
        {"LD A,(HL)", &CPU::LDAmHL, 8}, {"LD A,A", &CPU::LDAA, 4}, {"ADD A,B", &CPU::ADDAB, 4},
        {"ADD A,C", &CPU::ADDAC, 4}, {"ADD A,D", &CPU::ADDAD, 4}, {"ADD A,E", &CPU::ADDAE, 4},
        {"ADD A,H", &CPU::ADDAH, 4}, {"ADD A,L", &CPU::ADDAL, 4}, {"ADD A,(HL)", &CPU::ADDAmHL, 8},
        {"ADD A,A", &CPU::ADDAA, 4}, {"ADC A,B", &CPU::ADCAB, 4}, {"ADC A,C", &CPU::ADCAC, 4},
        {"ADC A,D", &CPU::ADCAD, 4}, {"ADC A,E", &CPU::ADCAE, 4}, {"ADC A,H", &CPU::ADCAH, 4},
        {"ADC A,L", &CPU::ADCAL, 4}, {"ADC A,(HL)", &CPU::ADCAmHL, 8}, {"ADC A,A", &CPU::ADCAA, 4},
        {"SUB B", &CPU::SUBB, 4}, {"SUB C", &CPU::SUBC, 4}, {"SUB D", &CPU::SUBD, 4},
        {"SUB E", &CPU::SUBE, 4}, {"SUB H", &CPU::SUBH, 4}, {"SUB L", &CPU::SUBL, 4},
        {"SUB (HL)", &CPU::SUBmHL, 8}, {"SUB A", &CPU::SUBA, 4}, {"SBC A,B", &CPU::SBCAB, 4},
        {"SBC A,C", &CPU::SBCAC, 4}, {"SBC A,D", &CPU::SBCAD, 4}, {"SBC A,E", &CPU::SBCAE, 4},
        {"SBC A,H", &CPU::SBCAH, 4}, {"SBC A,L", &CPU::SBCAL, 4}, {"SBC A,(HL)", &CPU::SBCAmHL, 8},
        {"SBC A,A", &CPU::SBCAA, 4}, {"AND B", &CPU::ANDB, 4}, {"AND C", &CPU::ANDC, 4}, 
        {"AND D", &CPU::ANDD, 4}, {"AND E", &CPU::ANDE, 4}, {"AND H", &CPU::ANDH, 4},
        {"AND L", &CPU::ANDL, 4}, {"AND (HL)", &CPU::ANDmHL, 8}, {"AND A", &CPU::ANDA, 4},
        {"XOR B", &CPU::XORB, 4}, {"XOR C", &CPU::XORC, 4}, {"XOR D", &CPU::XORD, 4},
        {"XOR E", &CPU::XORE, 4}, {"XOR H", &CPU::XORH, 4}, {"XOR L", &CPU::XORL, 4},
        {"XOR (HL)", &CPU::XORmHL, 8}, {"XOR A", &CPU::XORA, 4}, {"OR B", &CPU::ORB, 4},
        {"OR C", &CPU::ORC, 4}, {"OR D", &CPU::ORD, 4}, {"OR E", &CPU::ORE, 4},
        {"OR H", &CPU::ORH, 4}, {"OR L", &CPU::ORL, 4}, {"OR (HL)", &CPU::ORmHL, 8},
        {"OR A",  &CPU::ORA, 4}, {"CP B", &CPU::CPB, 4}, {"CP C", &CPU::CPC, 4},
        {"CP D", &CPU::CPD, 4}, {"CP E", &CPU::CPE, 4}, {"CP H", &CPU::CPH, 4},
        {"CP L", &CPU::CP_L, 4}, {"CP (HL)", &CPU::CPmHL, 8}, {"CP A", &CPU::CPA, 4},
        {"RETnz", &CPU::RETnz, 8}, {"POP BC", &CPU::POPBC, 12}, {"JPnz,nn", &CPU::JPnznn, 12},
        {"JPnn", &CPU::JPnn, 16}, {"CALLnz,nn", &CPU::CALLnznn, 12}, {"PUSH BC", &CPU::PUSHBC, 16},
        {"ADD A,n", &CPU::ADDAn, 8}, {"RST 00H", &CPU::RST00H, 16}, {"RETz", &CPU::RETz, 8},
        {"RET", &CPU::RET, 16}, {"JPz,nn", &CPU::JPznn, 12}, {"", &CPU::CBn, 4},
        {"CALLz,nn", &CPU::CALLznn, 12}, {"CALL nn", &CPU::CALLnn, 24}, {"ADC A,n", &CPU::ADCAn, 8},
        {"RST 08H", &CPU::RST08H, 16}, {"RETnc", &CPU::RETnc, 8}, {"POP DE", &CPU::POPDE, 12},
        {"JPnc,nn", &CPU::JPncnn, 12}, {"PANIC", &CPU::PANIC, 0}, {"CALLnc,nn", &CPU::CALLncnn, 12},
        {"PUSH DE", &CPU::PUSHDE, 16}, {"SUB n", &CPU::SUBAn, 8}, {"RST 10H", &CPU::RST10H, 16},
        {"RETc", &CPU::RETc, 8}, {"RETI", &CPU::RETI, 16}, {"JPc,nn", &CPU::JPcnn, 12},
        {"PANIC", &CPU::PANIC, 0}, {"CALLc,nn", &CPU::CALLcnn, 12}, {"PANIC", &CPU::PANIC, 0},
        {"SBC A,n", &CPU::SBCAn, 8}, {"RST 18H", &CPU::RST18H, 16}, {"LDH (n),A", &CPU::LDHmnA, 12},
        {"POP HL", &CPU::POPHL, 12}, {"LD (C),A", &CPU::LDmCA, 8}, {"PANIC", &CPU::PANIC, 0},
        {"PANIC", &CPU::PANIC, 0}, {"PUSH HL", &CPU::PUSHHL, 16}, {"AND n", &CPU::ANDn, 8},
        {"RST 20H", &CPU::RST20H, 16}, {"ADD SP,n", &CPU::ADDSPn, 16}, {"JP HL", &CPU::JPHL, 4},
        {"LD (nn),A", &CPU::LDmnnA, 16}, {"PANIC", &CPU::PANIC, 0}, {"PANIC", &CPU::PANIC, 0},
        {"PANIC", &CPU::PANIC, 0}, {"XOR n", &CPU::XORn, 8}, {"RST 28H", &CPU::RST28H, 16},
        {"LDH A,(n)", &CPU::LDHAmn, 12}, {"POP AF", &CPU::POPAF, 12}, {"LD A,(C)", &CPU::LDAmC, 8},
        {"DI", &CPU::DI, 4}, {"PANIC", &CPU::PANIC, 0}, {"PUSH AF", &CPU::PUSHAF, 16},
        {"OR n", &CPU::ORn, 8}, {"RST 30H", &CPU::RST30H, 16}, {"LD HL,SP+n", &CPU::LDHLSPn, 12},
        {"LD SP,HL", &CPU::LDSPHL, 8}, {"LD A,(nn)", &CPU::LDAmnn, 16}, {"EI", &CPU::EI, 4},
        {"PANIC", &CPU::PANIC, 0}, {"PANIC", &CPU::PANIC, 0}, {"CP n", &CPU::CPn, 8},
        {"RST 38H", &CPU::RST38H, 16}
    }};
    cb_rom = {{
        {"RLC B", &CPU::RLCB, 8}, {"RLC C", &CPU::RLCC, 8}, {"RLC D", &CPU::RLCD, 8},
        {"RLC E", &CPU::RLCE, 8}, {"RLC H", &CPU::RLCH, 8}, {"RLC L", &CPU::RLCL, 8},
        {"RLC (HL)", &CPU::RLCmHL, 16}, {"RLC A", &CPU::RLC_A, 8}, {"RRC B", &CPU::RRCB, 8},
        {"RRC C", &CPU::RRCC, 8}, {"RRC D", &CPU::RRCD, 8}, {"RRC E", &CPU::RRCE, 8},
        {"RRC H", &CPU::RRCH, 8}, {"RRC L", &CPU::RRCL, 8}, {"RRC (HL)", &CPU::RRCmHL, 16},
        {"RRC A", &CPU::RRC_A, 8},
        {"RL B", &CPU::RLB, 8}, {"RL C", &CPU::RLC, 8}, {"RL D", &CPU::RLD, 8},
        {"RL E", &CPU::RLE, 8}, {"RL H", &CPU::RLH, 8}, {"RL L", &CPU::RLL, 8},
        {"RL (HL)", &CPU::RLmHL, 16}, {"RL A", &CPU::RL_A, 8}, {"RR B", &CPU::RRB, 8},
        {"RR C", &CPU::RRC, 8}, {"RR D", &CPU::RRD, 8}, {"RR E", &CPU::RRE, 8},
        {"RR H", &CPU::RRH, 8}, {"RR L", &CPU::RRL, 8}, {"RR (HL)", &CPU::RRmHL, 16},
        {"RR A", &CPU::RR_A, 8},
        {"SLA B", &CPU::SLAB, 8}, {"SLA C", &CPU::SLAC, 8}, {"SLA D", &CPU::SLAD, 8},
        {"SLA E", &CPU::SLAE, 8}, {"SLA H", &CPU::SLAH, 8}, {"SLA L", &CPU::SLAL, 8},
        {"SLA (HL)", &CPU::SLAmHL, 16}, {"SLA A", &CPU::SLAA, 8}, {"SRA B", &CPU::SRAB, 8},
        {"SRA C", &CPU::SRAC, 8}, {"SRA D", &CPU::SRAD, 8}, {"SRA E", &CPU::SRAE, 8},
        {"SRA H", &CPU::SRAH, 8}, {"SRA L", &CPU::SRAL, 8}, {"SRA (HL)", &CPU::SRAmHL, 16},
        {"SRA A", &CPU::SRAA, 8},
        {"SWAP B", &CPU::SWAPB, 8}, {"SWAP C", &CPU::SWAPC, 8}, {"SWAP D", &CPU::SWAPD, 8},
        {"SWAP E", &CPU::SWAPE, 8}, {"SWAP H", &CPU::SWAPH, 8}, {"SWAP L", &CPU::SWAPL, 8},
        {"SWAP (HL)", &CPU::SWAPmHL, 16}, {"SWAP A", &CPU::SWAPA, 8}, {"SRL B", &CPU::SRLB, 8},
        {"SRL C", &CPU::SRLC, 8}, {"SRL D", &CPU::SRLD, 8}, {"SRL E", &CPU::SRLE, 8},
        {"SRL H", &CPU::SRLH, 8}, {"SRL L", &CPU::SRLL, 8}, {"SRL (HL)", &CPU::SRLmHL, 16},
        {"SRL A", &CPU::SRLA, 8},
        {"BIT 0,B", &CPU::BIT0B, 8}, {"BIT 0,C", &CPU::BIT0C, 8},
        {"BIT 0,D", &CPU::BIT0D, 8}, {"BIT 0,E", &CPU::BIT0E, 8}, {"BIT 0,H", &CPU::BIT0H, 8},
        {"BIT 0,L", &CPU::BIT0L, 8}, {"BIT 0,(HL)", &CPU::BIT0mHL, 16}, {"BIT 0,A", &CPU::BIT0A, 8},
        {"BIT 1,B", &CPU::BIT1B, 8}, {"BIT 1,C", &CPU::BIT1C, 8}, {"BIT 1,D", &CPU::BIT1D, 8},
        {"BIT 1,E", &CPU::BIT1E, 8}, {"BIT 1,H", &CPU::BIT1H, 8}, {"BIT 1,L", &CPU::BIT1L, 8},
        {"BIT 1,(HL)", &CPU::BIT1mHL, 8}, {"BIT 1,A", &CPU::BIT1A, 8}, {"BIT 2,B", &CPU::BIT2B, 8},
        {"BIT 2,C", &CPU::BIT2C, 8}, {"BIT 2,D", &CPU::BIT2D, 8}, {"BIT 2,E", &CPU::BIT2E, 8},
        {"BIT 2,H", &CPU::BIT2H, 8}, {"BIT 2,L", &CPU::BIT2L, 8}, {"BIT 2,(HL)", &CPU::BIT2mHL, 16},
        {"BIT 2,A", &CPU::BIT2A, 8}, {"BIT 3,B", &CPU::BIT3B, 8}, {"BIT 3,C", &CPU::BIT3C, 8},
        {"BIT 3,D", &CPU::BIT3D, 8}, {"BIT 3,E", &CPU::BIT3E, 8}, {"BIT 3,H", &CPU::BIT3H, 8},
        {"BIT 3,L", &CPU::BIT3L, 8}, {"BIT 3,(HL)", &CPU::BIT3mHL, 8}, {"BIT 3,A", &CPU::BIT3A, 8},
        {"BIT 4,B", &CPU::BIT4B, 8}, {"BIT 4,C", &CPU::BIT4C, 8}, {"BIT 4,D", &CPU::BIT4D, 8},
        {"BIT 4,E", &CPU::BIT4E, 8}, {"BIT 4,H", &CPU::BIT4H, 8}, {"BIT 4,L", &CPU::BIT4L, 8},
        {"BIT 4,(HL)", &CPU::BIT4mHL, 16}, {"BIT 4,A", &CPU::BIT4A, 8}, {"BIT 5,B", &CPU::BIT5B, 8},
        {"BIT 5,C", &CPU::BIT5C, 8}, {"BIT 5,D", &CPU::BIT5D, 8}, {"BIT 5,E", &CPU::BIT5E, 8},
        {"BIT 5,H", &CPU::BIT5H, 8}, {"BIT 5,L", &CPU::BIT5L, 8}, {"BIT 5,(HL)", &CPU::BIT5mHL, 8},
        {"BIT 5,A", &CPU::BIT5A, 8}, {"BIT 6,B", &CPU::BIT6B, 8}, {"BIT 6,C", &CPU::BIT6C, 8},
        {"BIT 6,D", &CPU::BIT6D, 8}, {"BIT 6,E", &CPU::BIT6E, 8}, {"BIT 6,H", &CPU::BIT6H, 8},
        {"BIT 6,L", &CPU::BIT6L, 8}, {"BIT 6,(HL)", &CPU::BIT6mHL, 16}, {"BIT 6,A", &CPU::BIT6A, 8},
        {"BIT 7,B", &CPU::BIT7B, 8}, {"BIT 7,C", &CPU::BIT7C, 8}, {"BIT 7,D", &CPU::BIT7D, 8},
        {"BIT 7,E", &CPU::BIT7E, 8}, {"BIT 7,H", &CPU::BIT7H, 8}, {"BIT 7,L", &CPU::BIT7L, 8},
        {"BIT 7,(HL)", &CPU::BIT7mHL, 8}, {"BIT 7,A", &CPU::BIT7A, 8}, {"RES 0,B", &CPU::RES0B, 8},
        {"RES 0,C", &CPU::RES0C, 8}, {"RES 0,D", &CPU::RES0D, 8}, {"RES 0,E", &CPU::RES0E, 8},
        {"RES 0,H", &CPU::RES0H, 8}, {"RES 0,L", &CPU::RES0L, 8}, {"RES 0,(HL)", &CPU::RES0mHL, 16},
        {"RES 0,A", &CPU::RES0A, 8}, {"RES 1,B", &CPU::RES1B, 8}, {"RES 1,C", &CPU::RES1C, 8},
        {"RES 1,D", &CPU::RES1D, 8}, {"RES 1,E", &CPU::RES1E, 8}, {"RES 1,H", &CPU::RES1H, 8},
        {"RES 1,L", &CPU::RES1L, 8}, {"RES 1,(HL)", &CPU::RES1mHL, 8}, {"RES 1,A", &CPU::RES1A, 8},
        {"RES 2,B", &CPU::RES2B, 8}, {"RES 2,C", &CPU::RES2C, 8}, {"RES 2,D", &CPU::RES2D, 8},
        {"RES 2,E", &CPU::RES2E, 8}, {"RES 2,H", &CPU::RES2H, 8}, {"RES 2,L", &CPU::RES2L, 8},
        {"RES 2,(HL)", &CPU::RES2mHL, 16}, {"RES 2,A", &CPU::RES2A, 8}, {"RES 3,B", &CPU::RES3B, 8},
        {"RES 3,C", &CPU::RES3C, 8}, {"RES 3,D", &CPU::RES3D, 8}, {"RES 3,E", &CPU::RES3E, 8},
        {"RES 3,H", &CPU::RES3H, 8}, {"RES 3,L", &CPU::RES3L, 8}, {"RES 3,(HL)", &CPU::RES3mHL, 8},
        {"RES 3,A", &CPU::RES3A, 8}, {"RES 4,B", &CPU::RES4B, 8}, {"RES 4,C", &CPU::RES4C, 8},
        {"RES 4,D", &CPU::RES4D, 8}, {"RES 4,E", &CPU::RES4E, 8}, {"RES 4,H", &CPU::RES4H, 8},
        {"RES 4,L", &CPU::RES4L, 8}, {"RES 4,(HL)", &CPU::RES4mHL, 16}, {"RES 4,A", &CPU::RES4A, 8},
        {"RES 5,B", &CPU::RES5B, 8}, {"RES 5,C", &CPU::RES5C, 8}, {"RES 5,D", &CPU::RES5D, 8},
        {"RES 5,E", &CPU::RES5E, 8}, {"RES 5,H", &CPU::RES5H, 8}, {"RES 5,L", &CPU::RES5L, 8},
        {"RES 5,(HL)", &CPU::RES5mHL, 8}, {"RES 5,A", &CPU::RES5A, 8}, {"RES 6,B", &CPU::RES6B, 8},
        {"RES 6,C", &CPU::RES6C, 8}, {"RES 6,D", &CPU::RES6D, 8}, {"RES 6,E", &CPU::RES6E, 8},
        {"RES 6,H", &CPU::RES6H, 8}, {"RES 6,L", &CPU::RES6L, 8}, {"RES 6,(HL)", &CPU::RES6mHL, 16},
        {"RES 6,A", &CPU::RES6A, 8}, {"RES 7,B", &CPU::RES7B, 8}, {"RES 7,C", &CPU::RES7C, 8},
        {"RES 7,D", &CPU::RES7D, 8}, {"RES 7,E", &CPU::RES7E, 8}, {"RES 7,H", &CPU::RES7H, 8},
        {"RES 7,L", &CPU::RES7L, 8}, {"RES 7,(HL)", &CPU::RES7mHL, 8}, {"RES 7,A", &CPU::RES7A, 8},
        {"SET 0,B", &CPU::SET0B, 8}, {"SET 0,C", &CPU::SET0C, 8}, {"SET 0,D", &CPU::SET0D, 8},
        {"SET 0,E", &CPU::SET0E, 8}, {"SET 0,H", &CPU::SET0H, 8}, {"SET 0,L", &CPU::SET0L, 8},
        {"SET 0,(HL)", &CPU::SET0mHL, 16}, {"SET 0,A", &CPU::SET0A, 8}, {"SET 1,B", &CPU::SET1B, 8},
        {"SET 1,C", &CPU::SET1C, 8}, {"SET 1,D", &CPU::SET1D, 8}, {"SET 1,E", &CPU::SET1E, 8},
        {"SET 1,H", &CPU::SET1H, 8}, {"SET 1,L", &CPU::SET1L, 8}, {"SET 1,(HL)", &CPU::SET1mHL, 8},
        {"SET 1,A", &CPU::SET1A, 8}, {"SET 2,B", &CPU::SET2B, 8}, {"SET 2,C", &CPU::SET2C, 8},
        {"SET 2,D", &CPU::SET2D, 8}, {"SET 2,E", &CPU::SET2E, 8}, {"SET 2,H", &CPU::SET2H, 8},
        {"SET 2,L", &CPU::SET2L, 8}, {"SET 2,(HL)", &CPU::SET2mHL, 16}, {"SET 2,A", &CPU::SET2A, 8},
        {"SET 3,B", &CPU::SET3B, 8}, {"SET 3,C", &CPU::SET3C, 8}, {"SET 3,D", &CPU::SET3D, 8},
        {"SET 3,E", &CPU::SET3E, 8}, {"SET 3,H", &CPU::SET3H, 8}, {"SET 3,L", &CPU::SET3L, 8},
        {"SET 3,(HL)", &CPU::SET3mHL, 8}, {"SET 3,A", &CPU::SET3A, 8}, {"SET 4,B", &CPU::SET4B, 8},
        {"SET 4,C", &CPU::SET4C, 8}, {"SET 4,D", &CPU::SET4D, 8}, {"SET 4,E", &CPU::SET4E, 8},
        {"SET 4,H", &CPU::SET4H, 8}, {"SET 4,L", &CPU::SET4L, 8}, {"SET 4,(HL)", &CPU::SET4mHL, 16},
        {"SET 4,A", &CPU::SET4A, 8}, {"SET 5,B", &CPU::SET5B, 8}, {"SET 5,C", &CPU::SET5C, 8},
        {"SET 5,D", &CPU::SET5D, 8}, {"SET 5,E", &CPU::SET5E, 8}, {"SET 5,H", &CPU::SET5H, 8},
        {"SET 5,L", &CPU::SET5L, 8}, {"SET 5,(HL)", &CPU::SET5mHL, 8}, {"SET 5,A", &CPU::SET5A, 8},
        {"SET 6,B", &CPU::SET6B, 8}, {"SET 6,C", &CPU::SET6C, 8}, {"SET 6,D", &CPU::SET6D, 8},
        {"SET 6,E", &CPU::SET6E, 8}, {"SET 6,H", &CPU::SET6H, 8}, {"SET 6,L", &CPU::SET6L, 8},
        {"SET 6,(HL)", &CPU::SET6mHL, 16}, {"SET 6,A", &CPU::SET6A, 8}, {"SET 7,B", &CPU::SET7B, 8},
        {"SET 7,C", &CPU::SET7C, 8}, {"SET 7,D", &CPU::SET7D, 8}, {"SET 7,E", &CPU::SET7E, 8},
        {"SET 7,H", &CPU::SET7H, 8}, {"SET 7,L", &CPU::SET7L, 8}, {"SET 7,(HL)", &CPU::SET7mHL, 8},
        {"SET 7,A", &CPU::SET7A, 8}
    }};
}

/**
 * @brief Operations to perform upon receiving a reset signal
 */
void CPU::reset() {
    PC = 0x0100;
    AF.r = 0x0000;
    BC.r = 0x0000;
    DE.r = 0x0000;
    HL.r = 0x0000;
    SP = 0xFFFE;
    intMaster = true;
    intFlags = 0x00;
    intEnable = 0x00;
    stop = false;
    halt = false;
    cycles = 0;
    timer.reset();
}

/**
 * Handle the next instruction.
 *
 * First the next opcode is fetched from memory, then this opcode is decoded and
 * executed using a jump table of function pointers which is indexed by the
 * opcode. After this, the number of cycles that instruction takes on the actual
 * MOS 6507 microprocessor is saved; this value is used for timing purposes.
 */
uint8_t CPU::step() {
    stepInterrupt();

    if (stop) {
        return 0x01;
    }
    if (halt) {
        return handleHalt();
    }
    opcode = fetch();
    cycles = this->instruction_rom[opcode].cycles;
    (this->*instruction_rom[opcode].op)();
    logInfo();
    return cycles;
}

uint8_t CPU::handleHalt() {
    if (intMaster) {
        if (intEnable & intFlags) {
            halt = false;
            push16(PC + 1);
            return 0x01;
        }
        opcode = 0x00; // simulate NOP
        cycles = this->instruction_rom[opcode].cycles;
        (this->*instruction_rom[opcode].op)();
        return cycles;
    }
    if (intEnable & intFlags) {
        // Halt bug
        uint16_t pc = PC;
        halt = false;
        opcode = fetch();
        cycles = this->instruction_rom[opcode].cycles;
        (this->*instruction_rom[opcode].op)();
        PC = pc;
        opcode = fetch();
        cycles = this->instruction_rom[opcode].cycles;
        (this->*instruction_rom[opcode].op)();
        return cycles;
    }
    opcode = 0x00;
    cycles = this->instruction_rom[opcode].cycles;
    (this->*instruction_rom[opcode].op)();
    return cycles;
}

//#define DEBUG

/**
 * @brief Print diagnostic information for debugging CPU execution.
 */
inline void CPU::logInfo() const {
#ifdef DEBUG
    std::cout << "PC = 0x" << std::hex << PC - 1 << ": "
        << std::setw(10) << (opcode == 0xCB ? cb_rom[opcode].name
        : instruction_rom[opcode].name) << " (0x"
        << std::setw(2) << std::hex << (int) opcode << ") AF: 0x" << std::setw(4)
        << std::hex << (int) AF.r << " BC: 0x" << std::setw(4) << std::hex
        << (int) BC.r << " DE: 0x" << std::setw(4) << std::hex << (int) DE.r
        << " HL: 0x" << std::setw(4) << std::hex << (int) HL.r << " SP: 0x"
        << std::setw(4) << std::hex << (int) SP << " IM: " << (int) intMaster
        << " IF: 0x" << std::setw(4) << std::hex << (int) intFlags
        << " IE: 0x" << std::setw(4) << std::hex << (int) intEnable
        << std::endl;
#endif
}

/**
 * @brief Fetch the next byte from RAM pointed to by the CPU's PC register.
 */
inline uint8_t CPU::fetch() { return read8(PC++); }

/**
 * @brief If negative, return offset and subtract 0x0100; otherwise just return
 * offset.
 */
inline uint16_t CPU::relativeOffset(uint8_t offset) const {
    return (uint16_t) offset + (((offset & SIGN) >> 7) * -0x0100);
}

inline void CPU::setBit(uint8_t f) { AF.r2 |= f; }
inline void CPU::clrBit(uint8_t f) { AF.r2 &= ~f; }

/**
 * @brief Set the ZERO and SIGN flags of the status register for the given parameter.
 */
inline void CPU::setZEROSIGN(uint8_t value) {
    value ? clrBit(ZERO) : setBit(ZERO);
    value & SIGN ? setBit(SIGN) : clrBit(SIGN);
}

void CPU::stepInterrupt() {
    if (intMaster) {
        uint8_t on = intEnable & intFlags;

        if (on) {
            intMaster = false;
            push16(PC);
            gb->gpu.clocks += 12;

            if (on & INT_VBLANK) {
                intFlags &= ~INT_VBLANK;
                vblankISR();
            } else if (on & INT_LCDC) {
                intFlags &= ~INT_LCDC;
                lcdcISR();
            } else if (on & INT_TIMER) {
                intFlags &= ~INT_TIMER;
                timerISR();
            } else if (on & INT_SERIAL) {
                intFlags &= ~INT_SERIAL;
                serialISR();
            } else if (on & INT_HITOLO) {
                intFlags &= ~INT_HITOLO;
                hitoloISR();
            }
        }
    }
}

void CPU::vblankISR() {
    PC = PC_VBLANK;
    gb->gpu.updateScreen();
}

void CPU::lcdcISR() { PC = PC_LCDC; }
void CPU::timerISR() { PC = PC_TIMER; }
void CPU::serialISR() { PC = PC_SERIAL; }
void CPU::hitoloISR() { PC = PC_HITOLO; }
uint8_t CPU::read8(uint16_t addr) { return gb->read8(addr); }
uint16_t CPU::read16(uint16_t addr) { return gb->read16(addr); }
void CPU::write8(uint16_t addr, uint8_t data) { gb->write8(addr, data); }
void CPU::write16(uint16_t addr, uint16_t data) { gb->write16(addr, data); }

uint16_t CPU::pop16() {
    uint16_t r = read16(SP);
    SP += 2;
    return r;
}

void CPU::push16(uint16_t data) {
    SP -= 2;
    write16(SP, data);
}

void CPU::_add8(uint8_t operand) {
    uint16_t sum = AF.r1 + operand;
    (((AF.r1 & 0x0F) + (operand & 0x0F)) > 0x0F) ? setBit(HALF)
        : clrBit(HALF);
    sum & 0x0100 ? setBit(CARRY) : clrBit(CARRY);
    sum & 0x00FF ? clrBit(ZERO) : setBit(ZERO);
    clrBit(SIGN);
    AF.r1 = sum & 0xFF;
}

void CPU::_add16(uint16_t operand) {
    uint32_t sum = HL.r + operand;
    sum & 0x10000 ? setBit(CARRY) : clrBit(CARRY);
    (((HL.r & 0x0FFF) + (operand & 0x0FFF)) > 0x0FFF) ? setBit(HALF)
        : clrBit(HALF);
    clrBit(SIGN);
    HL.r = sum & 0xFFFF;
}

void CPU::_adc(uint8_t operand) {
    uint8_t carry = (AF.r2 & CARRY) ? 1 : 0;
    uint16_t a = operand + AF.r1 + carry;
    a > 0xFF ? setBit(CARRY) : clrBit(CARRY);
    (((AF.r1 & 0x0F) + (operand & 0x0F) + carry) > 0x0F) ? setBit(HALF)
        : clrBit(HALF);
    AF.r1 = a & 0xFF;
    AF.r1 ? clrBit(ZERO) : setBit(ZERO);
    clrBit(SIGN);
}

void CPU::_sub(uint8_t operand) {
    operand > AF.r1 ? setBit(CARRY) : clrBit(CARRY);
    (operand & 0x0F) > (AF.r1 & 0x0F) ? setBit(HALF) : clrBit(HALF);
    AF.r1 -= operand;
    AF.r1 ? clrBit(ZERO) : setBit(ZERO);
    setBit(SIGN);
}

void CPU::_sbc(uint8_t operand) {
    uint8_t carry = (AF.r2 & CARRY) ? 1 : 0;
    int16_t diff = AF.r1 - operand - carry;
    ((AF.r1 & 0x0F) - (operand & 0x0F) - carry) < 0x00 ? setBit(HALF) : clrBit(HALF);
    diff < 0x00 ? setBit(CARRY) : clrBit(CARRY);
    AF.r1 = diff & 0xFF;
    AF.r1 ? clrBit(ZERO) : setBit(ZERO);
    setBit(SIGN);
}

void CPU::_and(uint8_t operand) {
    AF.r1 &= operand;
    AF.r1 ? clrBit(ZERO) : setBit(ZERO);
    setBit(HALF);
    clrBit(SIGN | CARRY);
}

void CPU::_xor(uint8_t operand) {
    AF.r1 ^= operand;
    AF.r1 ? clrBit(ZERO) : setBit(ZERO);
    clrBit(SIGN | CARRY | HALF);
}

void CPU::_or(uint8_t operand) {
    AF.r1 |= operand;
    AF.r1 ? clrBit(ZERO) : setBit(ZERO);
    clrBit(SIGN | CARRY | HALF);
}

void CPU::_cp(uint8_t operand) {
    AF.r1 == operand ? setBit(ZERO) : clrBit(ZERO);
    operand > AF.r1 ? setBit(CARRY) : clrBit(CARRY);
    (operand & 0x0F) > (AF.r1 & 0x0F) ? setBit(HALF) : clrBit(HALF);
    setBit(SIGN);
}

void CPU::_inc(uint8_t& operand) {
    operand++;
    (operand & 0x0F) == 0x00 ? setBit(HALF) : clrBit(HALF);
    operand ? clrBit(ZERO) : setBit(ZERO);
    clrBit(SIGN);
}

void CPU::_dec(uint8_t& operand) {
    operand--;
    (operand & 0x0F) == 0x0F ? setBit(HALF) : clrBit(HALF);
    operand ? clrBit(ZERO) : setBit(ZERO);
    setBit(SIGN);
}

void CPU::_rlc(uint8_t& operand) {
    uint8_t carry = (operand & 0x80) >> 7;
    carry ? setBit(CARRY) : clrBit(CARRY);
    operand = (operand << 1) | carry;
    operand ? clrBit(ZERO) : setBit(ZERO);
    clrBit(SIGN | HALF);
}

void CPU::_rrc(uint8_t& operand) {
    uint8_t carry = (operand & 0x1) << 7;
    carry ? setBit(CARRY) : clrBit(CARRY);
    operand = (operand >> 1) | carry;
    operand ? clrBit(ZERO) : setBit(ZERO);
    clrBit(SIGN | HALF);
}

void CPU::_rl(uint8_t& operand) {
    uint8_t carry = (AF.r2 & CARRY) ? 1 : 0;
    operand & 0x80 ? setBit(CARRY) : clrBit(CARRY);
    operand = (operand << 1) | carry;
    operand ? clrBit(ZERO) : setBit(ZERO);
    clrBit(SIGN | HALF);
}

void CPU::_rr(uint8_t& operand) {
    uint8_t carry = (AF.r2 & CARRY) ? 0x80 : 0x00;
    operand & 0x01 ? setBit(CARRY) : clrBit(CARRY);
    operand = (operand >> 1) | carry;
    operand ? clrBit(ZERO) : setBit(ZERO);
    clrBit(SIGN | HALF);
}

void CPU::_sla(uint8_t& operand) {
    operand & 0x80 ? setBit(CARRY) : clrBit(CARRY);
    operand <<= 1;
    operand ? clrBit(ZERO) : setBit(ZERO);
    clrBit(SIGN | HALF);
}

void CPU::_sra(uint8_t& operand) {
    operand & 0x01 ? setBit(CARRY) : clrBit(CARRY);
    operand = (operand & 0x80) | (operand >> 1);
    operand ? clrBit(ZERO) : setBit(ZERO);
    clrBit(SIGN | HALF);
}

void CPU::_swap(uint8_t& operand) {
    operand = ((operand & 0x0F) << 4) | ((operand & 0xF0) >> 4);
    operand ? clrBit(ZERO) : setBit(ZERO);
    clrBit(SIGN | CARRY | HALF);
}

void CPU::_srl(uint8_t& operand) {
    operand & 0x01 ? setBit(CARRY) : clrBit(CARRY);
    operand >>= 1;
    operand ? clrBit(ZERO) : setBit(ZERO);
    clrBit(SIGN | HALF);
}

void CPU::_bit(uint8_t mask, uint8_t operand) {
    operand & mask? clrBit(ZERO) : setBit(ZERO);
    clrBit(SIGN);
    setBit(HALF);
}

void CPU::PANIC() {
    std::cout << "Unknown opcode 0x" << std::hex << (int) opcode << std::endl;
    PC--;
}

/*******************************************************************************
 *                              Instruction Set                                *
 ******************************************************************************/
void CPU::NOP() { }

void CPU::LDBCnn() {
    BC.r = read16(PC);
    PC += 2;
}

void CPU::LDmBCA() { write8(BC.r, AF.r1); }
void CPU::INCBC() { BC.r++; }
void CPU::INCB() { _inc(BC.r1); }
void CPU::DECB() { _dec(BC.r1); }
void CPU::LDBn() { BC.r1 = fetch(); }

void CPU::RLCA() {
    uint8_t carry = (AF.r1 & 0x80) >> 7;
    carry ? setBit(CARRY) : clrBit(CARRY);
    AF.r1 = (AF.r1 << 1) | carry;
    clrBit(SIGN | HALF | ZERO);
}

void CPU::LDmnnSP() {
    write16(read16(PC), SP);
    PC += 2;
}

void CPU::ADDHLBC() { _add16(BC.r); }
void CPU::LDAmBC() { AF.r1 = read8(BC.r); }
void CPU::DECBC() { BC.r--; }
void CPU::INCC() { _inc(BC.r2); }
void CPU::DECC() { _dec(BC.r2); }
void CPU::LDCn() { BC.r2 = fetch(); }

void CPU::RRCA() {
    uint8_t carry = (AF.r1 & 0x01) << 7;
    carry ? setBit(CARRY) : clrBit(CARRY);
    AF.r1 = (AF.r1 >> 1) | carry;
    clrBit(SIGN | HALF | ZERO);
}

void CPU::STOP() { stop = 1; }

void CPU::LDDEnn() {
    DE.r = read16(PC);
    PC += 2;
}

void CPU::LDmDEA() { write8(DE.r, AF.r1); }
void CPU::INCDE() { DE.r++; }
void CPU::INCD() { _inc(DE.r1); }
void CPU::DECD() { _dec(DE.r1); }
void CPU::LDDn() { DE.r1 = fetch(); }

void CPU::RLA() {
    uint8_t carry = (AF.r2 & CARRY) ? 1 : 0;
    AF.r1 & 0x80 ? setBit(CARRY) : clrBit(CARRY);
    AF.r1 = (AF.r1 << 1) | carry;
    clrBit(SIGN | HALF | ZERO);
}

void CPU::JRn() { PC += (int8_t) fetch(); }
void CPU::ADDHLDE() { _add16(DE.r); }
void CPU::LDAmDE() { AF.r1 = read8(DE.r); }
void CPU::DECDE() { DE.r--; }
void CPU::INCE() { _inc(DE.r2); }
void CPU::DECE() { _dec(DE.r2); }
void CPU::LDEn() { DE.r2 = fetch(); }

void CPU::RRA() {
    uint8_t carry = (AF.r2 & CARRY) ? 0x80 : 0x00;
    (AF.r1 & 0x01) ? setBit(CARRY) : clrBit(CARRY);
    AF.r1 = (AF.r1 >> 1) | carry;
    clrBit(HALF | SIGN | ZERO);
}

void CPU::JRnzn() {
    int8_t offset = (int8_t) fetch();
    if (!(AF.r2 & ZERO)) {
        PC += offset;
        cycles += 4;
    }
}

void CPU::LDHLnn() {
    HL.r = read16(PC);
    PC += 2;
}

void CPU::LDmHLpA() { write8(HL.r++, AF.r1); }
void CPU::INCHL() { HL.r++; }
void CPU::INCH() { _inc(HL.r1); }
void CPU::DECH() { _dec(HL.r1); }
void CPU::LDHn() { HL.r1 = fetch(); }

void CPU::DAA() {
    int32_t a = AF.r1;

    if (!(AF.r2 & SIGN)) {
        if ((AF.r2 & HALF) || (a & 0x0F) > 9) {
            a += 0x06;
        }
        if ((AF.r2 & CARRY) || a > 0x9F) {
            a += 0x60;
        }
    } else {
        if (AF.r2 & HALF) {
            a = (a - 6) & 0xFF;
        }
        if (AF.r2 & CARRY) {
            a -= 0x60;
        }
    }
    if ((a & 0x0100) == 0x0100) {
        setBit(CARRY);
    }
    a &= 0xFF;
    a ? clrBit(ZERO) : setBit(ZERO);
    clrBit(HALF);
    AF.r1 = (uint8_t) a;
}

void CPU::JRzn() {
    int8_t offset = (int8_t) fetch();
    if (AF.r2 & ZERO) {
        PC += offset;
        cycles += 4;
    }
}

void CPU::ADDHLHL() { _add16(HL.r); }
void CPU::LDAmHLp() { AF.r1 = read8(HL.r++); }
void CPU::DECHL() { HL.r--; }
void CPU::INCL() { _inc(HL.r2); }
void CPU::DECL() { _dec(HL.r2); }
void CPU::LDLn() { HL.r2 = fetch(); }

void CPU::CPL() {
    AF.r1 = ~AF.r1;
    setBit(SIGN | HALF);
}

void CPU::JRncn() {
    int8_t offset = (int8_t) fetch();
    if (!(AF.r2 & CARRY)) {
        PC += offset;
        cycles += 4;
    }
}

void CPU::LDSPnn() {
    SP = read16(PC);
    PC += 2;
}

void CPU::LDmHLmA() { write8(HL.r--, AF.r1); }
void CPU::INCSP() { SP++; }

void CPU::INCmHL() {
    uint8_t operand = read8(HL.r);
    _inc(operand);
    write16(HL.r, operand);
}

void CPU::DECmHL() {
    uint8_t operand = read8(HL.r);
    _dec(operand);
    write16(HL.r, operand);
}

void CPU::LDmHLn() { write8(HL.r, fetch()); }

void CPU::SCF() {
    setBit(CARRY);
    clrBit(SIGN | HALF);
}

void CPU::JRcn() {
    int8_t offset = (int8_t) fetch();
    if (AF.r2 & CARRY) {
        PC += offset;
        cycles += 4;
    }
}

void CPU::ADDHLSP() { _add16(SP); }
void CPU::LDAmHLm() { AF.r1 = read8(HL.r--); }
void CPU::DECSP() { SP--; }
void CPU::INCA() { _inc(AF.r1); }
void CPU::DECA() { _dec(AF.r1); }
void CPU::LDAn() { AF.r1 = fetch(); }

void CPU::CCF() {
    (AF.r2 & CARRY) ? clrBit(CARRY) : setBit(CARRY);
    clrBit(SIGN | HALF);
}

void CPU::LDBB() { BC.r1 = BC.r1; }
void CPU::LDBC() { BC.r1 = BC.r2; }
void CPU::LDBD() { BC.r1 = DE.r1; }
void CPU::LDBE() { BC.r1 = DE.r2; }
void CPU::LDBH() { BC.r1 = HL.r1; }
void CPU::LDBL() { BC.r1 = HL.r2; }
void CPU::LDBmHL() { BC.r1 = read8(HL.r); }
void CPU::LDBA() { BC.r1 = AF.r1; }
void CPU::LDCB() { BC.r2 = BC.r1; }
void CPU::LDCC() { BC.r2 = BC.r2; }
void CPU::LDCD() { BC.r2 = DE.r1; }
void CPU::LDCE() { BC.r2 = DE.r2; }
void CPU::LDCH() { BC.r2 = HL.r1; }
void CPU::LDCL() { BC.r2 = HL.r2; }
void CPU::LDCmHL() { BC.r2 = read8(HL.r); }
void CPU::LDCA() { BC.r2 = AF.r1; }
void CPU::LDDB() { DE.r1 = BC.r1; }
void CPU::LDDC() { DE.r1 = BC.r2; }
void CPU::LDDD() { DE.r1 = DE.r1; }
void CPU::LDDE() { DE.r1 = DE.r2; }
void CPU::LDDH() { DE.r1 = HL.r1; }
void CPU::LDDL() { DE.r1 = HL.r2; }
void CPU::LDDmHL() { DE.r1 = read8(HL.r); }
void CPU::LDDA() { DE.r1 = AF.r1; }
void CPU::LDEB() { DE.r2 = BC.r1; }
void CPU::LDEC() { DE.r2 = BC.r2; }
void CPU::LDED() { DE.r2 = DE.r1; }
void CPU::LDEE() { DE.r2 = DE.r2; }
void CPU::LDEH() { DE.r2 = HL.r1; }
void CPU::LDEL() { DE.r2 = HL.r2; }
void CPU::LDEmHL() { DE.r2 = read8(HL.r); }
void CPU::LDEA() { DE.r2 = AF.r1; }
void CPU::LDHB() { HL.r1 = BC.r1; }
void CPU::LDHC() { HL.r1 = BC.r2; }
void CPU::LDHD() { HL.r1 = DE.r1; }
void CPU::LDHE() { HL.r1 = DE.r2; }
void CPU::LDHH() { HL.r1 = HL.r1; }
void CPU::LDHL() { HL.r1 = HL.r2; }
void CPU::LDHmHL() { HL.r1 = read8(HL.r); }
void CPU::LDHA() { HL.r1 = AF.r1; }
void CPU::LDLB() { HL.r2 = BC.r1; }
void CPU::LDLC() { HL.r2 = BC.r2; }
void CPU::LDLD() { HL.r2 = DE.r1; }
void CPU::LDLE() { HL.r2 = DE.r2; }
void CPU::LDLH() { HL.r2 = HL.r1; }
void CPU::LDLL() { HL.r2 = HL.r2; }
void CPU::LDLmHL() { HL.r2 = read8(HL.r); }
void CPU::LDLA() { HL.r2 = AF.r1; }
void CPU::LDmHLB() { write8(HL.r, BC.r1); }
void CPU::LDmHLC() { write8(HL.r, BC.r2); }
void CPU::LDmHLD() { write8(HL.r, DE.r1); }
void CPU::LDmHLE() { write8(HL.r, DE.r2); }
void CPU::LDmHLH() { write8(HL.r, HL.r1); }
void CPU::LDmHLL() { write8(HL.r, HL.r2); }
void CPU::HALT() { halt = 1; }
void CPU::LDmHLA() { write8(HL.r, AF.r1); }
void CPU::LDAB() { AF.r1 = BC.r1; }
void CPU::LDAC() { AF.r1 = BC.r2; }
void CPU::LDAD() { AF.r1 = DE.r1; }
void CPU::LDAE() { AF.r1 = DE.r2; }
void CPU::LDAH() { AF.r1 = HL.r1; }
void CPU::LDAL() { AF.r1 = HL.r2; }
void CPU::LDAmHL() { AF.r1 = read8(HL.r); }
void CPU::LDAA() { AF.r1 = AF.r1; }
void CPU::ADDAB() { _add8(BC.r1); }
void CPU::ADDAC() { _add8(BC.r2); }
void CPU::ADDAD() { _add8(DE.r1); }
void CPU::ADDAE() { _add8(DE.r2); }
void CPU::ADDAH() { _add8(HL.r1); }
void CPU::ADDAL() { _add8(HL.r2); }
void CPU::ADDAmHL() { _add8(read8(HL.r)); }
void CPU::ADDAA() { _add8(AF.r1); }
void CPU::ADCAB() { _adc(BC.r1); }
void CPU::ADCAC() { _adc(BC.r2); }
void CPU::ADCAD() { _adc(DE.r1); }
void CPU::ADCAE() { _adc(DE.r2); }
void CPU::ADCAH() { _adc(HL.r1); }
void CPU::ADCAL() { _adc(HL.r2); }
void CPU::ADCAmHL() { _adc(read8(HL.r)); }
void CPU::ADCAA() { _adc(AF.r1); }
void CPU::SUBB() { _sub(BC.r1); }
void CPU::SUBC() { _sub(BC.r2); }
void CPU::SUBD() { _sub(DE.r1); }
void CPU::SUBE() { _sub(DE.r2); }
void CPU::SUBH() { _sub(HL.r1); }
void CPU::SUBL() { _sub(HL.r2); }
void CPU::SUBmHL() { _sub(read8(HL.r)); }
void CPU::SUBA() { _sub(AF.r1); }
void CPU::SBCAB() { _sbc(BC.r1); }
void CPU::SBCAC() { _sbc(BC.r2); }
void CPU::SBCAD() { _sbc(DE.r1); }
void CPU::SBCAE() { _sbc(DE.r2); }
void CPU::SBCAH() { _sbc(HL.r1); }
void CPU::SBCAL() { _sbc(HL.r2); }
void CPU::SBCAmHL() { _sbc(read8(HL.r)); }
void CPU::SBCAA() { _sbc(AF.r1); }
void CPU::ANDB() { _and(BC.r1); }
void CPU::ANDC() { _and(BC.r2); }
void CPU::ANDD() { _and(DE.r1); }
void CPU::ANDE() { _and(DE.r2); }
void CPU::ANDH() { _and(HL.r1); }
void CPU::ANDL() { _and(HL.r2); }
void CPU::ANDmHL() { _and(read8(HL.r)); }
void CPU::ANDA() { _and(AF.r1); }
void CPU::XORB() { _xor(BC.r1); }
void CPU::XORC() { _xor(BC.r2); }
void CPU::XORD() { _xor(DE.r1); }
void CPU::XORE() { _xor(DE.r2); }
void CPU::XORH() { _xor(HL.r1); }
void CPU::XORL() { _xor(HL.r2); }
void CPU::XORmHL() { _xor(read8(HL.r)); }
void CPU::XORA() { _xor(AF.r1); }
void CPU::ORB() { _or(BC.r1); }
void CPU::ORC() { _or(BC.r2); }
void CPU::ORD() { _or(DE.r1); }
void CPU::ORE() { _or(DE.r2); }
void CPU::ORH() { _or(HL.r1); }
void CPU::ORL() { _or(HL.r2); }
void CPU::ORmHL() { _or(read8(HL.r)); }
void CPU::ORA() { _or(AF.r1); }
void CPU::CPB() { _cp(BC.r1); }
void CPU::CPC() { _cp(BC.r2); }
void CPU::CPD() { _cp(DE.r1); }
void CPU::CPE() { _cp(DE.r2); }
void CPU::CPH() { _cp(HL.r1); }
void CPU::CP_L() { _cp(HL.r2); }
void CPU::CPmHL() { _cp(read8(HL.r)); }
void CPU::CPA() { _cp(AF.r1); }

void CPU::RETnz() {
    if (!(AF.r2 & ZERO)) {
        PC = pop16();
        cycles += 12;
    }
}

void CPU::POPBC() { BC.r = pop16(); }

void CPU::JPnznn() {
    uint16_t operand = read16(PC);
    PC += 2;
    if (!(AF.r2 & ZERO)) {
        PC = operand;
        cycles += 4;
    }
}

void CPU::JPnn() { PC = read16(PC); }

void CPU::CALLnznn() {
    uint16_t operand = read16(PC);
    PC += 2;
    if (!(AF.r2 & ZERO)) {
        push16(PC);
        PC = operand;
        cycles += 12;
    }
}

void CPU::PUSHBC() { push16(BC.r); }
void CPU::ADDAn() { _add8(fetch()); }

void CPU::RST00H() {
    push16(PC);
    PC = 0x0000;
}

void CPU::RETz() {
    if (AF.r2 & ZERO) {
        PC = pop16();
        cycles += 12;
    }
}

void CPU::RET() { PC = pop16(); }

void CPU::JPznn() {
    uint16_t operand = read16(PC);
    PC += 2;
    if (AF.r2 & ZERO) {
        PC = operand;
        cycles += 4;
    }
}

void CPU::CBn() {
    opcode = fetch();
    (*this.*cb_rom[opcode].op)();
    cycles += cb_rom[opcode].cycles;
}

void CPU::CALLznn() {
    uint16_t operand = read16(PC);
    PC += 2;
    if (AF.r2 & ZERO) {
        push16(PC);
        PC = operand;
        cycles += 12;
    }
}

void CPU::CALLnn() {
    uint16_t operand = read16(PC);
    PC += 2;
    push16(PC);
    PC = operand;
}

void CPU::ADCAn() { _adc(fetch()); }

void CPU::RST08H() {
    push16(PC);
    PC = 0x0008;
}

void CPU::RETnc() {
    if (!(AF.r2 & CARRY)) {
        PC = pop16();
        cycles += 12;
    }
}

void CPU::POPDE() { DE.r = pop16(); }

void CPU::JPncnn() {
    uint16_t operand = read16(PC);
    PC += 2;
    if (!(AF.r2 & CARRY)) {
        PC = operand;
        cycles += 4;
    }
}

void CPU::CALLncnn() {
    uint16_t operand = read16(PC);
    PC += 2;
    if (!(AF.r2 & CARRY)) {
        push16(PC);
        PC = operand;
        cycles += 12;
    }
}

void CPU::PUSHDE() { push16(DE.r); }
void CPU::SUBAn() { _sub(fetch()); }

void CPU::RST10H() {
    push16(PC);
    PC = 0x0010;
}

void CPU::RETc() {
    if (AF.r2 & CARRY) {
        PC = pop16();
        cycles += 12;
    }
}

void CPU::RETI() {
    PC = pop16();
    intMaster = true;
}

void CPU::JPcnn() {
    uint16_t operand = read16(PC);
    PC += 2;
    if (AF.r2 & CARRY) {
        PC = operand;
        cycles += 4;
    }
}

void CPU::CALLcnn() {
    uint16_t operand = read16(PC);
    PC += 2;
    if (AF.r2 & CARRY) {
        push16(PC);
        PC = operand;
        cycles += 12;
    }
}

void CPU::SBCAn() { _sbc(fetch()); }

void CPU::RST18H() {
    push16(PC);
    PC = 0x0018;
}

void CPU::LDHmnA() { write8(0xFF00 + fetch(), AF.r1); }
void CPU::POPHL() { HL.r = pop16(); }
void CPU::LDmCA() { write8(0xFF00 + BC.r2, AF.r1); }
void CPU::PUSHHL() { push16(HL.r); }
void CPU::ANDn() { _and(fetch()); }

void CPU::RST20H() {
    push16(PC);
    PC = 0x0020;
}

void CPU::ADDSPn() {
    int8_t operand = (int8_t) fetch();
    int32_t sum = SP + operand;
    (((SP ^ operand ^ (sum & 0xFFFF)) & 0x0010) == 0x0010) ? setBit(HALF)
        : clrBit(HALF);
    (((SP ^ operand ^ (sum & 0xFFFF)) & 0x0100) == 0x0100) ? setBit(CARRY)
        : clrBit(CARRY);
    clrBit(SIGN | ZERO);
    SP = (uint16_t) sum;
}

void CPU::JPHL() { PC = HL.r; }

void CPU::LDmnnA() {
    uint16_t operand = read16(PC);
    PC += 2;
    write8(operand, AF.r1);
}

void CPU::XORn() { _xor(fetch()); }

void CPU::RST28H() {
    push16(PC);
    PC = 0x0028;
}

void CPU::LDHAmn() { AF.r1 = read8(0xFF00 + fetch()); }

void CPU::POPAF() {
    uint16_t af = pop16();
    AF.r1 = (af & 0xFF00) >> 8;
    AF.r2 = af & 0x00F0; // lower nibble of F cannot be modified
}

void CPU::LDAmC() { AF.r1 = read8(0xFF00 + BC.r2); }
void CPU::DI() { intMaster = false; }
void CPU::PUSHAF() { push16(AF.r); }
void CPU::ORn() { _or(fetch()); }

void CPU::RST30H() {
    push16(PC);
    PC = 0x0030;
}

void CPU::LDHLSPn() {
    int8_t operand = (int8_t) fetch();
    int32_t sum = SP + operand;

    (((SP ^ operand ^ (sum & 0xFFFF)) & 0x0010) == 0x0010) ? setBit(HALF)
        : clrBit(HALF);
    (((SP ^ operand ^ (sum & 0xFFFF)) & 0x0100) == 0x0100) ? setBit(CARRY)
        : clrBit(CARRY);
    clrBit(SIGN | ZERO);
    HL.r = sum & 0xFFFF;
}

void CPU::LDSPHL() { SP = HL.r; }

void CPU::LDAmnn() {
    uint16_t operand = read16(PC);
    PC += 2;
    AF.r1 = read8(operand);
}

void CPU::EI() { intMaster = true; }
void CPU::CPn() { _cp(fetch()); }

void CPU::RST38H() {
    push16(PC);
    PC = 0x0038;
}

void CPU::RLCB() { _rlc(BC.r1); }
void CPU::RLCC() { _rlc(BC.r2); }
void CPU::RLCD() { _rlc(DE.r1); }
void CPU::RLCE() { _rlc(DE.r2); }
void CPU::RLCH() { _rlc(HL.r1); }
void CPU::RLCL() { _rlc(HL.r2); }

void CPU::RLCmHL() {
    uint8_t operand = read8(HL.r);
    _rlc(operand);
    write8(HL.r, operand);
}

void CPU::RLC_A() { _rlc(AF.r1); }
void CPU::RRCB() { _rrc(BC.r1); }
void CPU::RRCC() { _rrc(BC.r2); }
void CPU::RRCD() { _rrc(DE.r1); }
void CPU::RRCE() { _rrc(DE.r2); }
void CPU::RRCH() { _rrc(HL.r1); }
void CPU::RRCL() { _rrc(HL.r2); }

void CPU::RRCmHL() {
    uint8_t operand = read8(HL.r);
    _rrc(operand);
    write8(HL.r, operand);
}

void CPU::RRC_A() { _rrc(AF.r1); }
void CPU::RLB() { _rl(BC.r1); }
void CPU::RLC() { _rl(BC.r2); }
void CPU::RLD() { _rl(DE.r1); }
void CPU::RLE() { _rl(DE.r2); }
void CPU::RLH() { _rl(HL.r1); }
void CPU::RLL() { _rl(HL.r2); }

void CPU::RLmHL() {
    uint8_t operand = read8(HL.r);
    _rl(operand);
    write8(HL.r, operand);
}

void CPU::RL_A() { _rl(AF.r1); }
void CPU::RRB() { _rr(BC.r1); }
void CPU::RRC() { _rr(BC.r2); }
void CPU::RRD() { _rr(DE.r1); }
void CPU::RRE() { _rr(DE.r2); }
void CPU::RRH() { _rr(HL.r1); }
void CPU::RRL() { _rr(HL.r2); }

void CPU::RRmHL() {
    uint8_t operand = read8(HL.r);
    _rr(operand);
    write8(HL.r, operand);
}

void CPU::RR_A() { _rr(AF.r1); }
void CPU::SLAB() { _sla(BC.r1); }
void CPU::SLAC() { _sla(BC.r2); }
void CPU::SLAD() { _sla(DE.r1); }
void CPU::SLAE() { _sla(DE.r2); }
void CPU::SLAH() { _sla(HL.r1); }
void CPU::SLAL() { _sla(HL.r2); }

void CPU::SLAmHL() {
    uint8_t operand = read8(HL.r);
    _sla(operand);
    write8(HL.r, operand);
}

void CPU::SLAA() { _sla(AF.r1); }
void CPU::SRAB() { _sra(BC.r1); }
void CPU::SRAC() { _sra(BC.r2); }
void CPU::SRAD() { _sra(DE.r1); }
void CPU::SRAE() { _sra(DE.r2); }
void CPU::SRAH() { _sra(HL.r1); }
void CPU::SRAL() { _sra(HL.r2); }

void CPU::SRAmHL() {
    uint8_t operand = read8(HL.r);
    _sra(operand);
    write8(HL.r, operand);
}

void CPU::SRAA() { _sra(AF.r1); }
void CPU::SWAPB() { _swap(BC.r1); }
void CPU::SWAPC() { _swap(BC.r2); }
void CPU::SWAPD() { _swap(DE.r1); }
void CPU::SWAPE() { _swap(DE.r2); }
void CPU::SWAPH() { _swap(HL.r1); }
void CPU::SWAPL() { _swap(HL.r2); }

void CPU::SWAPmHL() {
    uint8_t operand = read8(HL.r);
    _swap(operand);
    write8(HL.r, operand);
}

void CPU::SWAPA() { _swap(AF.r1); }
void CPU::SRLB() { _srl(BC.r1); }
void CPU::SRLC() { _srl(BC.r2); }
void CPU::SRLD() { _srl(DE.r1); }
void CPU::SRLE() { _srl(DE.r2); }
void CPU::SRLH() { _srl(HL.r1); }
void CPU::SRLL() { _srl(HL.r2); }

void CPU::SRLmHL() {
    uint8_t operand = read8(HL.r);
    _srl(operand);
    write8(HL.r, operand);
}

void CPU::SRLA() { _srl(AF.r1); }
void CPU::BIT0B() { _bit(1, BC.r1); }
void CPU::BIT0C() { _bit(1, BC.r2); }
void CPU::BIT0D() { _bit(1, DE.r1); }
void CPU::BIT0E() { _bit(1, DE.r2); }
void CPU::BIT0H() { _bit(1, HL.r1); }
void CPU::BIT0L() { _bit(1, HL.r2); }

void CPU::BIT0mHL() {
    uint8_t operand = read8(HL.r);
    _bit(1, operand);
    write8(HL.r, operand);
}

void CPU::BIT0A() { _bit(1, AF.r1); }
void CPU::BIT1B() { _bit(2, BC.r1); }
void CPU::BIT1C() { _bit(2, BC.r2); }
void CPU::BIT1D() { _bit(2, DE.r1); }
void CPU::BIT1E() { _bit(2, DE.r2); }
void CPU::BIT1H() { _bit(2, HL.r1); }
void CPU::BIT1L() { _bit(2, HL.r2); }

void CPU::BIT1mHL() {
    uint8_t operand = read8(HL.r);
    _bit(2, operand);
    write8(HL.r, operand);
}

void CPU::BIT1A() { _bit(2, AF.r1); }
void CPU::BIT2B() { _bit(4, BC.r1); }
void CPU::BIT2C() { _bit(4, BC.r2); }
void CPU::BIT2D() { _bit(4, DE.r1); }
void CPU::BIT2E() { _bit(4, DE.r2); }
void CPU::BIT2H() { _bit(4, HL.r1); }
void CPU::BIT2L() { _bit(4, HL.r2); }

void CPU::BIT2mHL() {
    uint8_t operand = read8(HL.r);
    _bit(4, operand);
    write8(HL.r, operand);
}

void CPU::BIT2A() { _bit(4, AF.r1); }
void CPU::BIT3B() { _bit(8, BC.r1); }
void CPU::BIT3C() { _bit(8, BC.r2); }
void CPU::BIT3D() { _bit(8, DE.r1); }
void CPU::BIT3E() { _bit(8, DE.r2); }
void CPU::BIT3H() { _bit(8, HL.r1); }
void CPU::BIT3L() { _bit(8, HL.r2); }

void CPU::BIT3mHL() {
    uint8_t operand = read8(HL.r);
    _bit(8, operand);
    write8(HL.r, operand);
}

void CPU::BIT3A() { _bit(8, AF.r1); }
void CPU::BIT4B() { _bit(16, BC.r1); }
void CPU::BIT4C() { _bit(16, BC.r2); }
void CPU::BIT4D() { _bit(16, DE.r1); }
void CPU::BIT4E() { _bit(16, DE.r2); }
void CPU::BIT4H() { _bit(16, HL.r1); }
void CPU::BIT4L() { _bit(16, HL.r2); }

void CPU::BIT4mHL() {
    uint8_t operand = read8(HL.r);
    _bit(16, operand);
    write8(HL.r, operand);
}

void CPU::BIT4A() { _bit(16, AF.r1); }
void CPU::BIT5B() { _bit(32, BC.r1); }
void CPU::BIT5C() { _bit(32, BC.r2); }
void CPU::BIT5D() { _bit(32, DE.r1); }
void CPU::BIT5E() { _bit(32, DE.r2); }
void CPU::BIT5H() { _bit(32, HL.r1); }
void CPU::BIT5L() { _bit(32, HL.r2); }

void CPU::BIT5mHL() {
    uint8_t operand = read8(HL.r);
    _bit(32, operand);
    write8(HL.r, operand);
}

void CPU::BIT5A() { _bit(32, AF.r1); }
void CPU::BIT6B() { _bit(64, BC.r1); }
void CPU::BIT6C() { _bit(64, BC.r2); }
void CPU::BIT6D() { _bit(64, DE.r1); }
void CPU::BIT6E() { _bit(64, DE.r2); }
void CPU::BIT6H() { _bit(64, HL.r1); }
void CPU::BIT6L() { _bit(64, HL.r2); }

void CPU::BIT6mHL() {
    uint8_t operand = read8(HL.r);
    _bit(64, operand);
    write8(HL.r, operand);
}

void CPU::BIT6A() { _bit(64, AF.r1); }
void CPU::BIT7B() { _bit(128, BC.r1); }
void CPU::BIT7C() { _bit(128, BC.r2); }
void CPU::BIT7D() { _bit(128, DE.r1); }
void CPU::BIT7E() { _bit(128, DE.r2); }
void CPU::BIT7H() { _bit(128, HL.r1); }
void CPU::BIT7L() { _bit(128, HL.r2); }

void CPU::BIT7mHL() {
    uint8_t operand = read8(HL.r);
    _bit(128, operand);
    write8(HL.r, operand);
}

void CPU::BIT7A() { _bit(128, AF.r1); }
void CPU::RES0B() { BC.r1 &= ~0x01; }
void CPU::RES0C() { BC.r2 &= ~0x01; }
void CPU::RES0D() { DE.r1 &= ~0x01; }
void CPU::RES0E() { DE.r2 &= ~0x01; }
void CPU::RES0H() { HL.r1 &= ~0x01; }
void CPU::RES0L() { HL.r2 &= ~0x01; }
void CPU::RES0mHL() { write8(HL.r, read8(HL.r) & ~0x01); }
void CPU::RES0A() { AF.r1 &= ~0x01; }
void CPU::RES1B() { BC.r1 &= ~0x02; }
void CPU::RES1C() { BC.r2 &= ~0x02; }
void CPU::RES1D() { DE.r1 &= ~0x02; }
void CPU::RES1E() { DE.r2 &= ~0x02; }
void CPU::RES1H() { HL.r1 &= ~0x02; }
void CPU::RES1L() { HL.r2 &= ~0x02; }
void CPU::RES1mHL() { write8(HL.r, read8(HL.r) & ~0x02); }
void CPU::RES1A() { AF.r1 &= ~0x02; }
void CPU::RES2B() { BC.r1 &= ~0x04; }
void CPU::RES2C() { BC.r2 &= ~0x04; }
void CPU::RES2D() { DE.r1 &= ~0x04; }
void CPU::RES2E() { DE.r2 &= ~0x04; }
void CPU::RES2H() { HL.r1 &= ~0x04; }
void CPU::RES2L() { HL.r2 &= ~0x04; }
void CPU::RES2mHL() { write8(HL.r, read8(HL.r) & ~0x04); }
void CPU::RES2A() { AF.r1 &= ~0x04; }
void CPU::RES3B() { BC.r1 &= ~0x08; }
void CPU::RES3C() { BC.r2 &= ~0x08; }
void CPU::RES3D() { DE.r1 &= ~0x08; }
void CPU::RES3E() { DE.r2 &= ~0x08; }
void CPU::RES3H() { HL.r1 &= ~0x08; }
void CPU::RES3L() { HL.r2 &= ~0x08; }
void CPU::RES3mHL() { write8(HL.r, read8(HL.r) & ~0x08); }
void CPU::RES3A() { AF.r1 &= ~0x08; }
void CPU::RES4B() { BC.r1 &= ~0x10; }
void CPU::RES4C() { BC.r2 &= ~0x10; }
void CPU::RES4D() { DE.r1 &= ~0x10; }
void CPU::RES4E() { DE.r2 &= ~0x10; }
void CPU::RES4H() { HL.r1 &= ~0x10; }
void CPU::RES4L() { HL.r2 &= ~0x10; }
void CPU::RES4mHL() { write8(HL.r, read8(HL.r) & ~0x10); }
void CPU::RES4A() { AF.r1 &= ~0x10; }
void CPU::RES5B() { BC.r1 &= ~0x20; }
void CPU::RES5C() { BC.r2 &= ~0x20; }
void CPU::RES5D() { DE.r1 &= ~0x20; }
void CPU::RES5E() { DE.r2 &= ~0x20; }
void CPU::RES5H() { HL.r1 &= ~0x20; }
void CPU::RES5L() { HL.r2 &= ~0x20; }
void CPU::RES5mHL() { write8(HL.r, read8(HL.r) & ~0x20); }
void CPU::RES5A() { AF.r1 &= ~0x20; }
void CPU::RES6B() { BC.r1 &= ~0x40; }
void CPU::RES6C() { BC.r2 &= ~0x40; }
void CPU::RES6D() { DE.r1 &= ~0x40; }
void CPU::RES6E() { DE.r2 &= ~0x40; }
void CPU::RES6H() { HL.r1 &= ~0x40; }
void CPU::RES6L() { HL.r2 &= ~0x40; }
void CPU::RES6mHL() { write8(HL.r, read8(HL.r) & ~0x40); }
void CPU::RES6A() { AF.r1 &= ~0x40; }
void CPU::RES7B() { BC.r1 &= ~0x80; }
void CPU::RES7C() { BC.r2 &= ~0x80; }
void CPU::RES7D() { DE.r1 &= ~0x80; }
void CPU::RES7E() { DE.r2 &= ~0x80; }
void CPU::RES7H() { HL.r1 &= ~0x80; }
void CPU::RES7L() { HL.r2 &= ~0x80; }
void CPU::RES7mHL() { write8(HL.r, read8(HL.r) & ~0x80); }
void CPU::RES7A() { AF.r1 &= ~0x80; }
void CPU::SET0B() { _setBit(1, BC.r1); }
void CPU::SET0C() { _setBit(1, BC.r2); }
void CPU::SET0D() { _setBit(1, DE.r1); }
void CPU::SET0E() { _setBit(1, DE.r2); }
void CPU::SET0H() { _setBit(1, HL.r1); }
void CPU::SET0L() { _setBit(1, HL.r2); }

void CPU::SET0mHL() {
    uint8_t operand = read8(HL.r);
    _setBit(1, operand);
    write8(HL.r, operand);
}

void CPU::SET0A() { _setBit(1, AF.r1); }
void CPU::SET1B() { _setBit(2, BC.r1); }
void CPU::SET1C() { _setBit(2, BC.r2); }
void CPU::SET1D() { _setBit(2, DE.r1); }
void CPU::SET1E() { exit(1); _setBit(2, DE.r2); }
void CPU::SET1H() { _setBit(2, HL.r1); }
void CPU::SET1L() { _setBit(2, HL.r2); }

void CPU::SET1mHL() {
    uint8_t operand = read8(HL.r);
    _setBit(2, operand);
    write8(HL.r, operand);
}

void CPU::SET1A() { _setBit(2, AF.r1); }
void CPU::SET2B() { _setBit(4, BC.r1); }
void CPU::SET2C() { _setBit(4, BC.r2); }
void CPU::SET2D() { _setBit(4, DE.r1); }
void CPU::SET2E() { _setBit(4, DE.r2); }
void CPU::SET2H() { _setBit(4, HL.r1); }
void CPU::SET2L() { _setBit(4, HL.r2); }

void CPU::SET2mHL() {
    uint8_t operand = read8(HL.r);
    _setBit(4, operand);
    write8(HL.r, operand);
}

void CPU::SET2A() { _setBit(4, AF.r1); }
void CPU::SET3B() { _setBit(8, BC.r1); }
void CPU::SET3C() { _setBit(8, BC.r2); }
void CPU::SET3D() { _setBit(8, DE.r1); }
void CPU::SET3E() { _setBit(8, DE.r2); }
void CPU::SET3H() { _setBit(8, HL.r1); }
void CPU::SET3L() { _setBit(8, HL.r2); }

void CPU::SET3mHL() {
    uint8_t operand = read8(HL.r);
    _setBit(8, operand);
    write8(HL.r, operand);
}

void CPU::SET3A() { _setBit(8, AF.r1); }
void CPU::SET4B() { _setBit(16, BC.r1); }
void CPU::SET4C() { _setBit(16, BC.r2); }
void CPU::SET4D() { _setBit(16, DE.r1); }
void CPU::SET4E() { _setBit(16, DE.r2); }
void CPU::SET4H() { _setBit(16, HL.r1); }
void CPU::SET4L() { _setBit(16, HL.r2); }

void CPU::SET4mHL() {
    uint8_t operand = read8(HL.r);
    _setBit(16, operand);
    write8(HL.r, operand);
}

void CPU::SET4A() { _setBit(16, AF.r1); }
void CPU::SET5B() { _setBit(32, BC.r1); }
void CPU::SET5C() { _setBit(32, BC.r2); }
void CPU::SET5D() { _setBit(32, DE.r1); }
void CPU::SET5E() { _setBit(32, DE.r2); }
void CPU::SET5H() { _setBit(32, HL.r1); }
void CPU::SET5L() { _setBit(32, HL.r2); }

void CPU::SET5mHL() {
    uint8_t operand = read8(HL.r);
    _setBit(32, operand);
    write8(HL.r, operand);
}

void CPU::SET5A() { _setBit(32, AF.r1); }
void CPU::SET6B() { _setBit(64, BC.r1); }
void CPU::SET6C() { _setBit(64, BC.r2); }
void CPU::SET6D() { _setBit(64, DE.r1); }
void CPU::SET6E() { _setBit(64, DE.r2); }
void CPU::SET6H() { _setBit(64, HL.r1); }
void CPU::SET6L() { _setBit(64, HL.r2); }

void CPU::SET6mHL() {
    uint8_t operand = read8(HL.r);
    _setBit(64, operand);
    write8(HL.r, operand);
}

void CPU::SET6A() { _setBit(64, AF.r1); }
void CPU::SET7B() { _setBit(128, BC.r1); }
void CPU::SET7C() { _setBit(128, BC.r2); }
void CPU::SET7D() { _setBit(128, DE.r1); }
void CPU::SET7E() { _setBit(128, DE.r2); }
void CPU::SET7H() { _setBit(128, HL.r1); }
void CPU::SET7L() { _setBit(128, HL.r2); }

void CPU::SET7mHL() {
    uint8_t operand = read8(HL.r);
    _setBit(128, operand);
    write8(HL.r, operand);
}

void CPU::SET7A() { _setBit(128, AF.r1); }

