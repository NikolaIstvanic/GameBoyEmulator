/**
 * CPU.cpp: class which programmatically recreates the GameBoy's Zilog z80 CPU
 */
#include <iomanip>
#include <iostream>

#include "GameBoy.hpp"
#include "CPU.hpp"

#define PAIR(_reg1, _reg2) ((uint16_t) ((_reg1) << 8) | (_reg2))

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
    A = 0x00;
    B = 0x00;
    C = 0x00;
    D = 0x00;
    E = 0x00;
    F = 0x00;
    H = 0x00;
    L = 0x00;
    PC = 0x0100;
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
        << std::hex << (int) PAIR(A, F) << " BC: 0x" << std::setw(4) << std::hex
        << (int) PAIR(B, C) << " DE: 0x" << std::setw(4) << std::hex
        << (int) PAIR(D, E) << " HL: 0x" << std::setw(4) << std::hex
        << (int) PAIR(H, L) << " SP: 0x" << std::setw(4) << std::hex
        << (int) SP << " IM: " << (int) intMaster << " IF: 0x" << std::setw(4)
        << std::hex << (int) intFlags << " IE: 0x" << std::setw(4) << std::hex
        << (int) intEnable << std::endl;
#endif
}

/**
 * @brief Fetch the next byte from RAM pointed to by the CPU's PC register.
 */
inline uint8_t CPU::fetch() { return read8(PC++); }
inline void CPU::setBit(uint8_t f) { F |= f; }
inline void CPU::clrBit(uint8_t f) { F &= ~f; }

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
    uint16_t sum = A + operand;
    (((A & 0x0F) + (operand & 0x0F)) > 0x0F) ? setBit(HALF)
        : clrBit(HALF);
    sum & 0x0100 ? setBit(CARRY) : clrBit(CARRY);
    sum & 0x00FF ? clrBit(ZERO) : setBit(ZERO);
    clrBit(SIGN);
    A = sum & 0x00FF;
}

void CPU::_add16(uint16_t operand) {
    uint32_t sum = PAIR(H, L) + operand;
    sum & 0x10000 ? setBit(CARRY) : clrBit(CARRY);
    (((PAIR(H, L) & 0x0FFF) + (operand & 0x0FFF)) > 0x0FFF) ? setBit(HALF)
        : clrBit(HALF);
    clrBit(SIGN);
    H = (sum & 0xFF00) >> 8;
    L = sum & 0x00FF;
}

void CPU::_adc(uint8_t operand) {
    uint8_t carry = (F & CARRY) ? 1 : 0;
    uint16_t a = operand + A + carry;
    a > 0xFF ? setBit(CARRY) : clrBit(CARRY);
    (((A & 0x0F) + (operand & 0x0F) + carry) > 0x0F) ? setBit(HALF)
        : clrBit(HALF);
    A = a & 0x00FF;
    A ? clrBit(ZERO) : setBit(ZERO);
    clrBit(SIGN);
}

void CPU::_sub(uint8_t operand) {
    operand > A ? setBit(CARRY) : clrBit(CARRY);
    (operand & 0x0F) > (A & 0x0F) ? setBit(HALF) : clrBit(HALF);
    A -= operand;
    A ? clrBit(ZERO) : setBit(ZERO);
    setBit(SIGN);
}

void CPU::_sbc(uint8_t operand) {
    uint8_t carry = (F & CARRY) ? 1 : 0;
    int16_t diff = A - operand - carry;
    ((A & 0x0F) - (operand & 0x0F) - carry) < 0x00 ? setBit(HALF) : clrBit(HALF);
    diff < 0x00 ? setBit(CARRY) : clrBit(CARRY);
    A = diff & 0x00FF;
    A ? clrBit(ZERO) : setBit(ZERO);
    setBit(SIGN);
}

void CPU::_and(uint8_t operand) {
    A &= operand;
    A ? clrBit(ZERO) : setBit(ZERO);
    setBit(HALF);
    clrBit(SIGN | CARRY);
}

void CPU::_xor(uint8_t operand) {
    A ^= operand;
    A ? clrBit(ZERO) : setBit(ZERO);
    clrBit(SIGN | CARRY | HALF);
}

void CPU::_or(uint8_t operand) {
    A |= operand;
    A ? clrBit(ZERO) : setBit(ZERO);
    clrBit(SIGN | CARRY | HALF);
}

void CPU::_cp(uint8_t operand) {
    A == operand ? setBit(ZERO) : clrBit(ZERO);
    operand > A ? setBit(CARRY) : clrBit(CARRY);
    (operand & 0x0F) > (A & 0x0F) ? setBit(HALF) : clrBit(HALF);
    setBit(SIGN);
}

uint8_t CPU::_inc(uint8_t operand) {
    operand++;
    (operand & 0x0F) == 0x00 ? setBit(HALF) : clrBit(HALF);
    operand ? clrBit(ZERO) : setBit(ZERO);
    clrBit(SIGN);
    return operand;
}

uint8_t CPU::_dec(uint8_t operand) {
    operand--;
    (operand & 0x0F) == 0x0F ? setBit(HALF) : clrBit(HALF);
    operand ? clrBit(ZERO) : setBit(ZERO);
    setBit(SIGN);
    return operand;
}

uint8_t CPU::_rlc(uint8_t operand) {
    uint8_t carry = (operand & 0x80) >> 7;
    carry ? setBit(CARRY) : clrBit(CARRY);
    operand = (operand << 1) | carry;
    operand ? clrBit(ZERO) : setBit(ZERO);
    clrBit(SIGN | HALF);
    return operand;
}

uint8_t CPU::_rrc(uint8_t operand) {
    uint8_t carry = (operand & 0x01) << 7;
    carry ? setBit(CARRY) : clrBit(CARRY);
    operand = (operand >> 1) | carry;
    operand ? clrBit(ZERO) : setBit(ZERO);
    clrBit(SIGN | HALF);
    return operand;
}

uint8_t CPU::_rl(uint8_t operand) {
    uint8_t carry = (F & CARRY) ? 1 : 0;
    operand & 0x80 ? setBit(CARRY) : clrBit(CARRY);
    operand = (operand << 1) | carry;
    operand ? clrBit(ZERO) : setBit(ZERO);
    clrBit(SIGN | HALF);
    return operand;
}

uint8_t CPU::_rr(uint8_t operand) {
    uint8_t carry = (F & CARRY) ? 0x80 : 0x00;
    operand & 0x01 ? setBit(CARRY) : clrBit(CARRY);
    operand = (operand >> 1) | carry;
    operand ? clrBit(ZERO) : setBit(ZERO);
    clrBit(SIGN | HALF);
    return operand;
}

uint8_t CPU::_sla(uint8_t operand) {
    operand & 0x80 ? setBit(CARRY) : clrBit(CARRY);
    operand <<= 1;
    operand ? clrBit(ZERO) : setBit(ZERO);
    clrBit(SIGN | HALF);
    return operand;
}

uint8_t CPU::_sra(uint8_t operand) {
    operand & 0x01 ? setBit(CARRY) : clrBit(CARRY);
    operand = (operand & 0x80) | (operand >> 1);
    operand ? clrBit(ZERO) : setBit(ZERO);
    clrBit(SIGN | HALF);
    return operand;
}

uint8_t CPU::_swap(uint8_t operand) {
    operand = ((operand & 0x0F) << 4) | ((operand & 0xF0) >> 4);
    operand ? clrBit(ZERO) : setBit(ZERO);
    clrBit(SIGN | CARRY | HALF);
    return operand;
}

uint8_t CPU::_srl(uint8_t operand) {
    operand & 0x01 ? setBit(CARRY) : clrBit(CARRY);
    operand >>= 1;
    operand ? clrBit(ZERO) : setBit(ZERO);
    clrBit(SIGN | HALF);
    return operand;
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
    uint16_t operand = read16(PC);
    PC += 2;
    B = (operand & 0xFF00) >> 8;
    C = operand & 0x00FF;
}

void CPU::LDmBCA() { write8(PAIR(B, C), A); }

void CPU::INCBC() {
    C++;
    if (!C) { B++; }
}

void CPU::INCB() { B = _inc(B); }
void CPU::DECB() { B = _dec(B); }
void CPU::LDBn() { B = fetch(); }

void CPU::RLCA() {
    uint8_t carry = (A & 0x80) >> 7;
    carry ? setBit(CARRY) : clrBit(CARRY);
    A = (A << 1) | carry;
    clrBit(SIGN | HALF | ZERO);
}

void CPU::LDmnnSP() {
    write16(read16(PC), SP);
    PC += 2;
}

void CPU::ADDHLBC() { _add16(PAIR(B, C)); }
void CPU::LDAmBC() { A = read8(PAIR(B, C)); }

void CPU::DECBC() {
    if (!C) { B--; }
    C--;
}

void CPU::INCC() { C = _inc(C); }
void CPU::DECC() { C = _dec(C); }
void CPU::LDCn() { C = fetch(); }

void CPU::RRCA() {
    uint8_t carry = (A & 0x01) << 7;
    carry ? setBit(CARRY) : clrBit(CARRY);
    A = (A >> 1) | carry;
    clrBit(SIGN | HALF | ZERO);
}

void CPU::STOP() { stop = 1; }

void CPU::LDDEnn() {
    uint16_t de = read16(PC);
    PC += 2;
    D = (de & 0xFF00) >> 8;
    E = de & 0x00FF;
}

void CPU::LDmDEA() { write8(PAIR(D, E), A); }

void CPU::INCDE() {
    E++;
    if (!E) { D++; }
}

void CPU::INCD() { D = _inc(D); }
void CPU::DECD() { D = _dec(D); }
void CPU::LDDn() { D = fetch(); }

void CPU::RLA() {
    uint8_t carry = (F & CARRY) ? 1 : 0;
    A & 0x80 ? setBit(CARRY) : clrBit(CARRY);
    A = (A << 1) | carry;
    clrBit(SIGN | HALF | ZERO);
}

void CPU::JRn() { PC += (int8_t) fetch(); }
void CPU::ADDHLDE() { _add16(PAIR(D, E)); }
void CPU::LDAmDE() { A = read8(PAIR(D, E)); }

void CPU::DECDE() {
    if (!E) { D--; }
    E--;
}

void CPU::INCE() { E = _inc(E); }
void CPU::DECE() { E = _dec(E); }
void CPU::LDEn() { E = fetch(); }

void CPU::RRA() {
    uint8_t carry = (F & CARRY) ? 0x80 : 0x00;
    A & 0x01 ? setBit(CARRY) : clrBit(CARRY);
    A = (A >> 1) | carry;
    clrBit(HALF | SIGN | ZERO);
}

void CPU::JRnzn() {
    int8_t offset = (int8_t) fetch();
    if (!(F & ZERO)) {
        PC += offset;
        cycles += 4;
    }
}

void CPU::LDHLnn() {
    uint16_t hl = read16(PC);
    PC += 2;
    H = (hl & 0xFF00) >> 8;
    L = hl & 0x00FF;
}

void CPU::LDmHLpA() {
    write8(PAIR(H, L), A);
    L++;
    if (!L) { H++; }
}

void CPU::INCHL() {
    L++;
    if (!L) { H++; }
}

void CPU::INCH() { H = _inc(H); }
void CPU::DECH() { H = _dec(H); }
void CPU::LDHn() { H = fetch(); }

void CPU::DAA() {
    int32_t a = A;

    if (!(F & SIGN)) {
        if ((F & HALF) || (a & 0x0F) > 9) {
            a += 0x06;
        }
        if ((F & CARRY) || a > 0x9F) {
            a += 0x60;
        }
    } else {
        if (F & HALF) {
            a = (a - 6) & 0xFF;
        }
        if (F & CARRY) {
            a -= 0x60;
        }
    }
    if ((a & 0x0100) == 0x0100) {
        setBit(CARRY);
    }
    a &= 0xFF;
    a ? clrBit(ZERO) : setBit(ZERO);
    clrBit(HALF);
    A = (uint8_t) a;
}

void CPU::JRzn() {
    int8_t offset = (int8_t) fetch();
    if (F & ZERO) {
        PC += offset;
        cycles += 4;
    }
}

void CPU::ADDHLHL() { _add16(PAIR(H, L)); }

void CPU::LDAmHLp() {
    A = read8(PAIR(H, L));
    L++;
    if (!L) { H++; }
}

void CPU::DECHL() {
    if (!L) { H--; }
    L--;
}

void CPU::INCL() { L = _inc(L); }
void CPU::DECL() { L = _dec(L); }
void CPU::LDLn() { L = fetch(); }

void CPU::CPL() {
    A = ~A;
    setBit(SIGN | HALF);
}

void CPU::JRncn() {
    int8_t offset = (int8_t) fetch();
    if (!(F & CARRY)) {
        PC += offset;
        cycles += 4;
    }
}

void CPU::LDSPnn() {
    SP = read16(PC);
    PC += 2;
}

void CPU::LDmHLmA() {
    write8(PAIR(H, L), A);
    if (!L) { H--; }
    L--;
}

void CPU::INCSP() { SP++; }
void CPU::INCmHL() { write8(PAIR(H, L), _inc(read8(PAIR(H, L)))); }
void CPU::DECmHL() { write8(PAIR(H, L), _dec(read8(PAIR(H, L)))); }
void CPU::LDmHLn() { write8(PAIR(H, L), fetch()); }

void CPU::SCF() {
    setBit(CARRY);
    clrBit(SIGN | HALF);
}

void CPU::JRcn() {
    int8_t offset = (int8_t) fetch();
    if (F & CARRY) {
        PC += offset;
        cycles += 4;
    }
}

void CPU::ADDHLSP() { _add16(SP); }
void CPU::LDAmHLm() {
    A = read8(PAIR(H, L));
    if (!L) { H--; }
    L--;
}

void CPU::DECSP() { SP--; }
void CPU::INCA() { A = _inc(A); }
void CPU::DECA() { A = _dec(A); }
void CPU::LDAn() { A = fetch(); }

void CPU::CCF() {
    (F & CARRY) ? clrBit(CARRY) : setBit(CARRY);
    clrBit(SIGN | HALF);
}

void CPU::LDBB() { }
void CPU::LDBC() { B = C; }
void CPU::LDBD() { B = D; }
void CPU::LDBE() { B = E; }
void CPU::LDBH() { B = H; }
void CPU::LDBL() { B = L; }
void CPU::LDBmHL() { B = read8(PAIR(H, L)); }
void CPU::LDBA() { B = A; }
void CPU::LDCB() { C = B; }
void CPU::LDCC() { }
void CPU::LDCD() { C = D; }
void CPU::LDCE() { C = E; }
void CPU::LDCH() { C = H; }
void CPU::LDCL() { C = L; }
void CPU::LDCmHL() { C = read8(PAIR(H, L)); }
void CPU::LDCA() { C = A; }
void CPU::LDDB() { D = B; }
void CPU::LDDC() { D = C; }
void CPU::LDDD() { }
void CPU::LDDE() { D = E; }
void CPU::LDDH() { D = H; }
void CPU::LDDL() { D = L; }
void CPU::LDDmHL() { D = read8(PAIR(H, L)); }
void CPU::LDDA() { D = A; }
void CPU::LDEB() { E = B; }
void CPU::LDEC() { E = C; }
void CPU::LDED() { E = D; }
void CPU::LDEE() { }
void CPU::LDEH() { E = H; }
void CPU::LDEL() { E = L; }
void CPU::LDEmHL() { E = read8(PAIR(H, L)); }
void CPU::LDEA() { E = A; }
void CPU::LDHB() { H = B; }
void CPU::LDHC() { H = C; }
void CPU::LDHD() { H = D; }
void CPU::LDHE() { H = E; }
void CPU::LDHH() { }
void CPU::LDHL() { H = L; }
void CPU::LDHmHL() { H = read8(PAIR(H, L)); }
void CPU::LDHA() { H = A; }
void CPU::LDLB() { L = B; }
void CPU::LDLC() { L = C; }
void CPU::LDLD() { L = D; }
void CPU::LDLE() { L = E; }
void CPU::LDLH() { L = H; }
void CPU::LDLL() { }
void CPU::LDLmHL() { L = read8(PAIR(H, L)); }
void CPU::LDLA() { L = A; }
void CPU::LDmHLB() { write8(PAIR(H, L), B); }
void CPU::LDmHLC() { write8(PAIR(H, L), C); }
void CPU::LDmHLD() { write8(PAIR(H, L), D); }
void CPU::LDmHLE() { write8(PAIR(H, L), E); }
void CPU::LDmHLH() { write8(PAIR(H, L), H); }
void CPU::LDmHLL() { write8(PAIR(H, L), L); }
void CPU::HALT() { halt = 1; }
void CPU::LDmHLA() { write8(PAIR(H, L), A); }
void CPU::LDAB() { A = B; }
void CPU::LDAC() { A = C; }
void CPU::LDAD() { A = D; }
void CPU::LDAE() { A = E; }
void CPU::LDAH() { A = H; }
void CPU::LDAL() { A = L; }
void CPU::LDAmHL() { A = read8(PAIR(H, L)); }
void CPU::LDAA() { }
void CPU::ADDAB() { _add8(B); }
void CPU::ADDAC() { _add8(C); }
void CPU::ADDAD() { _add8(D); }
void CPU::ADDAE() { _add8(E); }
void CPU::ADDAH() { _add8(H); }
void CPU::ADDAL() { _add8(L); }
void CPU::ADDAmHL() { _add8(read8(PAIR(H, L))); }
void CPU::ADDAA() { _add8(A); }
void CPU::ADCAB() { _adc(B); }
void CPU::ADCAC() { _adc(C); }
void CPU::ADCAD() { _adc(D); }
void CPU::ADCAE() { _adc(E); }
void CPU::ADCAH() { _adc(H); }
void CPU::ADCAL() { _adc(L); }
void CPU::ADCAmHL() { _adc(read8(PAIR(H, L))); }
void CPU::ADCAA() { _adc(A); }
void CPU::SUBB() { _sub(B); }
void CPU::SUBC() { _sub(C); }
void CPU::SUBD() { _sub(D); }
void CPU::SUBE() { _sub(E); }
void CPU::SUBH() { _sub(H); }
void CPU::SUBL() { _sub(L); }
void CPU::SUBmHL() { _sub(read8(PAIR(H, L))); }
void CPU::SUBA() { _sub(A); }
void CPU::SBCAB() { _sbc(B); }
void CPU::SBCAC() { _sbc(C); }
void CPU::SBCAD() { _sbc(D); }
void CPU::SBCAE() { _sbc(E); }
void CPU::SBCAH() { _sbc(H); }
void CPU::SBCAL() { _sbc(L); }
void CPU::SBCAmHL() { _sbc(read8(PAIR(H, L))); }
void CPU::SBCAA() { _sbc(A); }
void CPU::ANDB() { _and(B); }
void CPU::ANDC() { _and(C); }
void CPU::ANDD() { _and(D); }
void CPU::ANDE() { _and(E); }
void CPU::ANDH() { _and(H); }
void CPU::ANDL() { _and(L); }
void CPU::ANDmHL() { _and(read8(PAIR(H, L))); }
void CPU::ANDA() { _and(A); }
void CPU::XORB() { _xor(B); }
void CPU::XORC() { _xor(C); }
void CPU::XORD() { _xor(D); }
void CPU::XORE() { _xor(E); }
void CPU::XORH() { _xor(H); }
void CPU::XORL() { _xor(L); }
void CPU::XORmHL() { _xor(read8(PAIR(H, L))); }
void CPU::XORA() { _xor(A); }
void CPU::ORB() { _or(B); }
void CPU::ORC() { _or(C); }
void CPU::ORD() { _or(D); }
void CPU::ORE() { _or(E); }
void CPU::ORH() { _or(H); }
void CPU::ORL() { _or(L); }
void CPU::ORmHL() { _or(read8(PAIR(H, L))); }
void CPU::ORA() { _or(A); }
void CPU::CPB() { _cp(B); }
void CPU::CPC() { _cp(C); }
void CPU::CPD() { _cp(D); }
void CPU::CPE() { _cp(E); }
void CPU::CPH() { _cp(H); }
void CPU::CP_L() { _cp(L); }
void CPU::CPmHL() { _cp(read8(PAIR(H, L))); }
void CPU::CPA() { _cp(A); }

void CPU::RETnz() {
    if (!(F & ZERO)) {
        PC = pop16();
        cycles += 12;
    }
}

void CPU::POPBC() {
    uint16_t bc = pop16();
    B = (bc & 0xFF00) >> 8;
    C = bc & 0x00FF;
}

void CPU::JPnznn() {
    uint16_t operand = read16(PC);
    PC += 2;
    if (!(F & ZERO)) {
        PC = operand;
        cycles += 4;
    }
}

void CPU::JPnn() { PC = read16(PC); }

void CPU::CALLnznn() {
    uint16_t operand = read16(PC);
    PC += 2;
    if (!(F & ZERO)) {
        push16(PC);
        PC = operand;
        cycles += 12;
    }
}

void CPU::PUSHBC() { push16(PAIR(B, C)); }
void CPU::ADDAn() { _add8(fetch()); }

void CPU::RST00H() {
    push16(PC);
    PC = 0x0000;
}

void CPU::RETz() {
    if (F & ZERO) {
        PC = pop16();
        cycles += 12;
    }
}

void CPU::RET() { PC = pop16(); }

void CPU::JPznn() {
    uint16_t operand = read16(PC);
    PC += 2;
    if (F & ZERO) {
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
    if (F & ZERO) {
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
    if (!(F & CARRY)) {
        PC = pop16();
        cycles += 12;
    }
}

void CPU::POPDE() {
    uint16_t de = pop16();
    D = (de & 0xFF00) >> 8;
    E = de & 0x00FF;
}

void CPU::JPncnn() {
    uint16_t operand = read16(PC);
    PC += 2;
    if (!(F & CARRY)) {
        PC = operand;
        cycles += 4;
    }
}

void CPU::CALLncnn() {
    uint16_t operand = read16(PC);
    PC += 2;
    if (!(F & CARRY)) {
        push16(PC);
        PC = operand;
        cycles += 12;
    }
}

void CPU::PUSHDE() { push16(PAIR(D, E)); }
void CPU::SUBAn() { _sub(fetch()); }

void CPU::RST10H() {
    push16(PC);
    PC = 0x0010;
}

void CPU::RETc() {
    if (F & CARRY) {
        PC = pop16();
        cycles += 12;
    }
}

void CPU::RETI() {
    intMaster = true;
    PC = pop16();
}

void CPU::JPcnn() {
    uint16_t operand = read16(PC);
    PC += 2;
    if (F & CARRY) {
        PC = operand;
        cycles += 4;
    }
}

void CPU::CALLcnn() {
    uint16_t operand = read16(PC);
    PC += 2;
    if (F & CARRY) {
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

void CPU::LDHmnA() { write8(0xFF00 + fetch(), A); }

void CPU::POPHL() {
    uint16_t hl = pop16();
    H = (hl & 0xFF00) >> 8;
    L = hl & 0x00FF;
}

void CPU::LDmCA() { write8(0xFF00 + C, A); }
void CPU::PUSHHL() { push16(PAIR(H, L)); }
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

void CPU::JPHL() { PC = PAIR(H, L); }

void CPU::LDmnnA() {
    uint16_t operand = read16(PC);
    PC += 2;
    write8(operand, A);
}

void CPU::XORn() { _xor(fetch()); }

void CPU::RST28H() {
    push16(PC);
    PC = 0x0028;
}

void CPU::LDHAmn() { A = read8(0xFF00 + fetch()); }

void CPU::POPAF() {
    uint16_t af = pop16();
    A = (af & 0xFF00) >> 8;
    F = af & 0x00F0; // lower nibble of F cannot be modified
}

void CPU::LDAmC() { A = read8(0xFF00 + C); }
void CPU::DI() { intMaster = false; }
void CPU::PUSHAF() { push16(PAIR(A, F)); }
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
    H = (sum & 0xFF00) >> 8;
    L = sum & 0x00FF;
}

void CPU::LDSPHL() { SP = PAIR(H, L); }

void CPU::LDAmnn() {
    uint16_t operand = read16(PC);
    PC += 2;
    A = read8(operand);
}

void CPU::EI() { intMaster = true; }
void CPU::CPn() { _cp(fetch()); }

void CPU::RST38H() {
    push16(PC);
    PC = 0x0038;
}

void CPU::RLCB() { B = _rlc(B); }
void CPU::RLCC() { C = _rlc(C); }
void CPU::RLCD() { D = _rlc(D); }
void CPU::RLCE() { E = _rlc(E); }
void CPU::RLCH() { H = _rlc(H); }
void CPU::RLCL() { L = _rlc(L); }
void CPU::RLCmHL() { write8(PAIR(H, L), _rlc(read8(PAIR(H, L)))); }
void CPU::RLC_A() { A = _rlc(A); }
void CPU::RRCB() { B = _rrc(B); }
void CPU::RRCC() { C = _rrc(C); }
void CPU::RRCD() { D = _rrc(D); }
void CPU::RRCE() { E = _rrc(E); }
void CPU::RRCH() { H = _rrc(H); }
void CPU::RRCL() { L = _rrc(L); }
void CPU::RRCmHL() { write8(PAIR(H, L), _rrc(read8(PAIR(H, L)))); }
void CPU::RRC_A() { A = _rrc(A); }
void CPU::RLB() { B = _rl(B); }
void CPU::RLC() { C = _rl(C); }
void CPU::RLD() { D = _rl(D); }
void CPU::RLE() { E = _rl(E); }
void CPU::RLH() { H = _rl(H); }
void CPU::RLL() { L = _rl(L); }
void CPU::RLmHL() { write8(PAIR(H, L), _rl(read8(PAIR(H, L)))); }
void CPU::RL_A() { A = _rl(A); }
void CPU::RRB() { B = _rr(B); }
void CPU::RRC() { C = _rr(C); }
void CPU::RRD() { D = _rr(D); }
void CPU::RRE() { E = _rr(E); }
void CPU::RRH() { H = _rr(H); }
void CPU::RRL() { L = _rr(L); }
void CPU::RRmHL() { write8(PAIR(H, L), _rr(read8(PAIR(H, L)))); }
void CPU::RR_A() { A = _rr(A); }
void CPU::SLAB() { B = _sla(B); }
void CPU::SLAC() { C = _sla(C); }
void CPU::SLAD() { D = _sla(D); }
void CPU::SLAE() { E = _sla(E); }
void CPU::SLAH() { H = _sla(H); }
void CPU::SLAL() { L = _sla(L); }
void CPU::SLAmHL() { write8(PAIR(H, L), _sla(read8(PAIR(H, L)))); }
void CPU::SLAA() { A = _sla(A); }
void CPU::SRAB() { B = _sra(B); }
void CPU::SRAC() { C = _sra(C); }
void CPU::SRAD() { D = _sra(D); }
void CPU::SRAE() { E = _sra(E); }
void CPU::SRAH() { H = _sra(H); }
void CPU::SRAL() { L = _sra(L); }
void CPU::SRAmHL() { write8(PAIR(H, L), _sra(read8(PAIR(H, L)))); }
void CPU::SRAA() { A = _sra(A); }
void CPU::SWAPB() { B = _swap(B); }
void CPU::SWAPC() { C = _swap(C); }
void CPU::SWAPD() { D = _swap(D); }
void CPU::SWAPE() { E = _swap(E); }
void CPU::SWAPH() { H = _swap(H); }
void CPU::SWAPL() { L = _swap(L); }
void CPU::SWAPmHL() { write8(PAIR(H, L), _swap(read8(PAIR(H, L)))); }
void CPU::SWAPA() { A = _swap(A); }
void CPU::SRLB() { B = _srl(B); }
void CPU::SRLC() { C = _srl(C); }
void CPU::SRLD() { D = _srl(D); }
void CPU::SRLE() { E = _srl(E); }
void CPU::SRLH() { H = _srl(H); }
void CPU::SRLL() { L = _srl(L); }
void CPU::SRLmHL() { write8(PAIR(H, L), _srl(read8(PAIR(H, L)))); }
void CPU::SRLA() { A = _srl(A); }
void CPU::BIT0B() { _bit(1, B); }
void CPU::BIT0C() { _bit(1, C); }
void CPU::BIT0D() { _bit(1, D); }
void CPU::BIT0E() { _bit(1, E); }
void CPU::BIT0H() { _bit(1, H); }
void CPU::BIT0L() { _bit(1, L); }
void CPU::BIT0mHL() { _bit(1, read8(PAIR(H, L))); }
void CPU::BIT0A() { _bit(1, A); }
void CPU::BIT1B() { _bit(2, B); }
void CPU::BIT1C() { _bit(2, C); }
void CPU::BIT1D() { _bit(2, D); }
void CPU::BIT1E() { _bit(2, E); }
void CPU::BIT1H() { _bit(2, H); }
void CPU::BIT1L() { _bit(2, L); }
void CPU::BIT1mHL() { _bit(2, read8(PAIR(H, L))); }
void CPU::BIT1A() { _bit(2, A); }
void CPU::BIT2B() { _bit(4, B); }
void CPU::BIT2C() { _bit(4, C); }
void CPU::BIT2D() { _bit(4, D); }
void CPU::BIT2E() { _bit(4, E); }
void CPU::BIT2H() { _bit(4, H); }
void CPU::BIT2L() { _bit(4, L); }
void CPU::BIT2mHL() { _bit(4, read8(PAIR(H, L))); }
void CPU::BIT2A() { _bit(4, A); }
void CPU::BIT3B() { _bit(8, B); }
void CPU::BIT3C() { _bit(8, C); }
void CPU::BIT3D() { _bit(8, D); }
void CPU::BIT3E() { _bit(8, E); }
void CPU::BIT3H() { _bit(8, H); }
void CPU::BIT3L() { _bit(8, L); }
void CPU::BIT3mHL() { _bit(8, read8(PAIR(H, L))); }
void CPU::BIT3A() { _bit(8, A); }
void CPU::BIT4B() { _bit(16, B); }
void CPU::BIT4C() { _bit(16, C); }
void CPU::BIT4D() { _bit(16, D); }
void CPU::BIT4E() { _bit(16, E); }
void CPU::BIT4H() { _bit(16, H); }
void CPU::BIT4L() { _bit(16, L); }
void CPU::BIT4mHL() { _bit(16, read8(PAIR(H, L))); }
void CPU::BIT4A() { _bit(16, A); }
void CPU::BIT5B() { _bit(32, B); }
void CPU::BIT5C() { _bit(32, C); }
void CPU::BIT5D() { _bit(32, D); }
void CPU::BIT5E() { _bit(32, E); }
void CPU::BIT5H() { _bit(32, H); }
void CPU::BIT5L() { _bit(32, L); }
void CPU::BIT5mHL() { _bit(32, read8(PAIR(H, L))); }
void CPU::BIT5A() { _bit(32, A); }
void CPU::BIT6B() { _bit(64, B); }
void CPU::BIT6C() { _bit(64, C); }
void CPU::BIT6D() { _bit(64, D); }
void CPU::BIT6E() { _bit(64, E); }
void CPU::BIT6H() { _bit(64, H); }
void CPU::BIT6L() { _bit(64, L); }
void CPU::BIT6mHL() { _bit(64, read8(PAIR(H, L))); }
void CPU::BIT6A() { _bit(64, A); }
void CPU::BIT7B() { _bit(128, B); }
void CPU::BIT7C() { _bit(128, C); }
void CPU::BIT7D() { _bit(128, D); }
void CPU::BIT7E() { _bit(128, E); }
void CPU::BIT7H() { _bit(128, H); }
void CPU::BIT7L() { _bit(128, L); }
void CPU::BIT7mHL() { _bit(128, read8(PAIR(H, L))); }
void CPU::BIT7A() { _bit(128, A); }
void CPU::RES0B() { B &= ~0x01; }
void CPU::RES0C() { C &= ~0x01; }
void CPU::RES0D() { D &= ~0x01; }
void CPU::RES0E() { E &= ~0x01; }
void CPU::RES0H() { H &= ~0x01; }
void CPU::RES0L() { L &= ~0x01; }
void CPU::RES0mHL() { write8(PAIR(H, L), read8(PAIR(H, L)) & ~0x01); }
void CPU::RES0A() { A &= ~0x01; }
void CPU::RES1B() { B &= ~0x02; }
void CPU::RES1C() { C &= ~0x02; }
void CPU::RES1D() { D &= ~0x02; }
void CPU::RES1E() { E &= ~0x02; }
void CPU::RES1H() { H &= ~0x02; }
void CPU::RES1L() { L &= ~0x02; }
void CPU::RES1mHL() { write8(PAIR(H, L), read8(PAIR(H, L)) & ~0x02); }
void CPU::RES1A() { A &= ~0x02; }
void CPU::RES2B() { B &= ~0x04; }
void CPU::RES2C() { C &= ~0x04; }
void CPU::RES2D() { D &= ~0x04; }
void CPU::RES2E() { E &= ~0x04; }
void CPU::RES2H() { H &= ~0x04; }
void CPU::RES2L() { L &= ~0x04; }
void CPU::RES2mHL() { write8(PAIR(H, L), read8(PAIR(H, L)) & ~0x04); }
void CPU::RES2A() { A &= ~0x04; }
void CPU::RES3B() { B &= ~0x08; }
void CPU::RES3C() { C &= ~0x08; }
void CPU::RES3D() { D &= ~0x08; }
void CPU::RES3E() { E &= ~0x08; }
void CPU::RES3H() { H &= ~0x08; }
void CPU::RES3L() { L &= ~0x08; }
void CPU::RES3mHL() { write8(PAIR(H, L), read8(PAIR(H, L)) & ~0x08); }
void CPU::RES3A() { A &= ~0x08; }
void CPU::RES4B() { B &= ~0x10; }
void CPU::RES4C() { C &= ~0x10; }
void CPU::RES4D() { D &= ~0x10; }
void CPU::RES4E() { E &= ~0x10; }
void CPU::RES4H() { H &= ~0x10; }
void CPU::RES4L() { L &= ~0x10; }
void CPU::RES4mHL() { write8(PAIR(H, L), read8(PAIR(H, L)) & ~0x10); }
void CPU::RES4A() { A &= ~0x10; }
void CPU::RES5B() { B &= ~0x20; }
void CPU::RES5C() { C &= ~0x20; }
void CPU::RES5D() { D &= ~0x20; }
void CPU::RES5E() { E &= ~0x20; }
void CPU::RES5H() { H &= ~0x20; }
void CPU::RES5L() { L &= ~0x20; }
void CPU::RES5mHL() { write8(PAIR(H, L), read8(PAIR(H, L)) & ~0x20); }
void CPU::RES5A() { A &= ~0x20; }
void CPU::RES6B() { B &= ~0x40; }
void CPU::RES6C() { C &= ~0x40; }
void CPU::RES6D() { D &= ~0x40; }
void CPU::RES6E() { E &= ~0x40; }
void CPU::RES6H() { H &= ~0x40; }
void CPU::RES6L() { L &= ~0x40; }
void CPU::RES6mHL() { write8(PAIR(H, L), read8(PAIR(H, L)) & ~0x40); }
void CPU::RES6A() { A &= ~0x40; }
void CPU::RES7B() { B &= ~0x80; }
void CPU::RES7C() { C &= ~0x80; }
void CPU::RES7D() { D &= ~0x80; }
void CPU::RES7E() { E &= ~0x80; }
void CPU::RES7H() { H &= ~0x80; }
void CPU::RES7L() { L &= ~0x80; }
void CPU::RES7mHL() { write8(PAIR(H, L), read8(PAIR(H, L)) & ~0x80); }
void CPU::RES7A() { A &= ~0x80; }
void CPU::SET0B() { B = _setBit(1, B); }
void CPU::SET0C() { C = _setBit(1, C); }
void CPU::SET0D() { D = _setBit(1, D); }
void CPU::SET0E() { E = _setBit(1, E); }
void CPU::SET0H() { H = _setBit(1, H); }
void CPU::SET0L() { L = _setBit(1, L); }
void CPU::SET0mHL() { write8(PAIR(H, L), _setBit(1, read8(PAIR(H, L)))); }
void CPU::SET0A() { A = _setBit(1, A); }
void CPU::SET1B() { B = _setBit(2, B); }
void CPU::SET1C() { C = _setBit(2, C); }
void CPU::SET1D() { D = _setBit(2, D); }
void CPU::SET1E() { E = _setBit(2, E); }
void CPU::SET1H() { H = _setBit(2, H); }
void CPU::SET1L() { L = _setBit(2, L); }
void CPU::SET1mHL() { write8(PAIR(H, L), _setBit(2, read8(PAIR(H, L)))); }
void CPU::SET1A() { A = _setBit(2, A); }
void CPU::SET2B() { B = _setBit(4, B); }
void CPU::SET2C() { C = _setBit(4, C); }
void CPU::SET2D() { D = _setBit(4, D); }
void CPU::SET2E() { E = _setBit(4, E); }
void CPU::SET2H() { H = _setBit(4, H); }
void CPU::SET2L() { L = _setBit(4, L); }
void CPU::SET2mHL() { write8(PAIR(H, L), _setBit(4, read8(PAIR(H, L)))); }
void CPU::SET2A() { A = _setBit(4, A); }
void CPU::SET3B() { B = _setBit(8, B); }
void CPU::SET3C() { C = _setBit(8, C); }
void CPU::SET3D() { D = _setBit(8, D); }
void CPU::SET3E() { E = _setBit(8, E); }
void CPU::SET3H() { H = _setBit(8, H); }
void CPU::SET3L() { L = _setBit(8, L); }
void CPU::SET3mHL() { write8(PAIR(H, L), _setBit(8, read8(PAIR(H, L)))); }
void CPU::SET3A() { A = _setBit(8, A); }
void CPU::SET4B() { B = _setBit(16, B); }
void CPU::SET4C() { C = _setBit(16, C); }
void CPU::SET4D() { D = _setBit(16, D); }
void CPU::SET4E() { E = _setBit(16, E); }
void CPU::SET4H() { H = _setBit(16, H); }
void CPU::SET4L() { L = _setBit(16, L); }
void CPU::SET4mHL() { write8(PAIR(H, L), _setBit(16, read8(PAIR(H, L)))); }
void CPU::SET4A() { A = _setBit(16, A); }
void CPU::SET5B() { B = _setBit(32, B); }
void CPU::SET5C() { C = _setBit(32, C); }
void CPU::SET5D() { D = _setBit(32, D); }
void CPU::SET5E() { E = _setBit(32, E); }
void CPU::SET5H() { H = _setBit(32, H); }
void CPU::SET5L() { L = _setBit(32, L); }
void CPU::SET5mHL() { write8(PAIR(H, L), _setBit(32, read8(PAIR(H, L)))); }
void CPU::SET5A() { A = _setBit(32, A); }
void CPU::SET6B() { B = _setBit(64, B); }
void CPU::SET6C() { C = _setBit(64, C); }
void CPU::SET6D() { D = _setBit(64, D); }
void CPU::SET6E() { E = _setBit(64, E); }
void CPU::SET6H() { H = _setBit(64, H); }
void CPU::SET6L() { L = _setBit(64, L); }
void CPU::SET6mHL() { write8(PAIR(H, L), _setBit(64, read8(PAIR(H, L)))); }
void CPU::SET6A() { A = _setBit(64, A); }
void CPU::SET7B() { B = _setBit(128, B); }
void CPU::SET7C() { C = _setBit(128, C); }
void CPU::SET7D() { D = _setBit(128, D); }
void CPU::SET7E() { E = _setBit(128, E); }
void CPU::SET7H() { H = _setBit(128, H); }
void CPU::SET7L() { L = _setBit(128, L); }
void CPU::SET7mHL() { write8(PAIR(H, L), _setBit(128, read8(PAIR(H, L)))); }
void CPU::SET7A() { A = _setBit(128, A); }

