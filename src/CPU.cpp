/**
 * CPU.cpp: class which programmatically recreates the GameBoy's Zilog z80 CPU
 */
#include <iomanip>
#include <iostream>

#include "GameBoy.hpp"
#include "CPU.hpp"

#define PC_VBLANK 0x0040
#define PC_LCDC   0x0048
#define PC_TIMER  0x0050
#define PC_SERIAL 0x0058
#define PC_HITOLO 0x0060

#define PAIR(_reg1, _reg2) ((uint16_t) ((_reg1) << 8) | (_reg2))

CPU::CPU() {
    m_instRom = {{
        {"NOP",        &CPU::NOP,      4}, {"LD BC,nn",   &CPU::LDBCnn,   12}, {"LD (BC),A",  &CPU::LDmBCA,    8},
        {"INC BC",     &CPU::INCBC,    8}, {"INC B",      &CPU::INCB,      4}, {"DEC B",      &CPU::DECB,      4},
        {"LD B,n",     &CPU::LDBn,     8}, {"RLCA",       &CPU::RLCA,      4}, {"LD (nn),SP", &CPU::LDmnnSP,  20},
        {"ADD HL,BC",  &CPU::ADDHLBC,  8}, {"LD A,(BC)",  &CPU::LDAmBC,    8}, {"DEC BC",     &CPU::DECBC,     8},
        {"INC C",      &CPU::INCC,     4}, {"DEC C",      &CPU::DECC,      4}, {"LD C,n",     &CPU::LDCn,      8},
        {"RRCA",       &CPU::RRCA,     4}, {"STOP",       &CPU::STOP,      4}, {"LD DE,nn",   &CPU::LDDEnn,   12},
        {"LD (DE),A",  &CPU::LDmDEA,   8}, {"INC DE",     &CPU::INCDE,     8}, {"INC D",      &CPU::INCD,      4},
        {"DEC D",      &CPU::DECD,     4}, {"LD D,n",     &CPU::LDDn,      8}, {"RLA",        &CPU::RLA,       4},
        {"JR n",       &CPU::JRn,     12}, {"ADD HL,DE",  &CPU::ADDHLDE,   8}, {"LD A,(DE)",  &CPU::LDAmDE,    8},
        {"DEC DE",     &CPU::DECDE,    8}, {"INC E",      &CPU::INCE,      4}, {"DEC E",      &CPU::DECE,      4},
        {"LD E,n",     &CPU::LDEn,     8}, {"RRA",        &CPU::RRA,       4}, {"JRnz,n",     &CPU::JRnzn,     8},
        {"LD HL,nn",   &CPU::LDHLnn,  12}, {"LD (HL+),A", &CPU::LDmHLpA,   8}, {"INC HL",     &CPU::INCHL,     8},
        {"INC H",      &CPU::INCH,     4}, {"DEC H",      &CPU::DECH,      4}, {"LD H,n",     &CPU::LDHn,      8},
        {"DAA",        &CPU::DAA,      4}, {"JRz,n",      &CPU::JRzn,      8}, {"ADD HL,HL",  &CPU::ADDHLHL,   8},
        {"LD A,(HL+)", &CPU::LDAmHLp,  8}, {"DEC HL",     &CPU::DECHL,     8}, {"INC L",      &CPU::INCL,      4},
        {"DEC L",      &CPU::DECL,     4}, {"LD L,n",     &CPU::LDLn,      8}, {"CPL",        &CPU::CPL,       4},
        {"JRnc,n",     &CPU::JRncn,    8}, {"LD SP,nn",   &CPU::LDSPnn,   12}, {"LD (HL-),A", &CPU::LDmHLmA,   8},
        {"INC SP",     &CPU::INCSP,    8}, {"INC (HL)",   &CPU::INCmHL,   12}, {"DEC (HL)",   &CPU::DECmHL,   12},
        {"LD (HL),n",  &CPU::LDmHLn,  12}, {"SCF",        &CPU::SCF,       4}, {"JRc,n",      &CPU::JRcn,      8},
        {"ADD HL,SP",  &CPU::ADDHLSP,  8}, {"LD A,(HL-)", &CPU::LDAmHLm,   8}, {"DEC SP",     &CPU::DECSP,     8},
        {"INC A",      &CPU::INCA,     4}, {"DEC A",      &CPU::DECA,      4}, {"LD A,n",     &CPU::LDAn,      8},
        {"CCF",        &CPU::CCF,      4}, {"LD B,B",     &CPU::LDBB,      4}, {"LD B,C",     &CPU::LDBC,      4},
        {"LD B,D",     &CPU::LDBD,     4}, {"LD B,E",     &CPU::LDBE,      4}, {"LD B,H",     &CPU::LDBH,      4},
        {"LD B,L",     &CPU::LDBL,     4}, {"LD B,(HL)",  &CPU::LDBmHL,    8}, {"LD B,A",     &CPU::LDBA,      4},
        {"LD C,B",     &CPU::LDCB,     4}, {"LD C,C",     &CPU::LDCC,      4}, {"LD C,D",     &CPU::LDCD,      4},
        {"LD C,E",     &CPU::LDCE,     4}, {"LD C,H",     &CPU::LDCH,      4}, {"LD C,L",     &CPU::LDCL,      4},
        {"LD C,(HL)",  &CPU::LDCmHL,   8}, {"LD C,A",     &CPU::LDCA,      4}, {"LD D,B",     &CPU::LDDB,      4},
        {"LD D,C",     &CPU::LDDC,     4}, {"LD D,D",     &CPU::LDDD,      4}, {"LD D,E",     &CPU::LDDE,      4},
        {"LD D,H",     &CPU::LDDH,     4}, {"LD D,L",     &CPU::LDDL,      4}, {"LD D,(HL)",  &CPU::LDDmHL,    8},
        {"LD D,A",     &CPU::LDDA,     4}, {"LD E,B",     &CPU::LDEB,      4}, {"LD E,C",     &CPU::LDEC,      4},
        {"LD E,D",     &CPU::LDED,     4}, {"LD E,E",     &CPU::LDEE,      4}, {"LD E,H",     &CPU::LDEH,      4},
        {"LD E,L",     &CPU::LDEL,     4}, {"LD E,(HL)",  &CPU::LDEmHL,    8}, {"LD E,A",     &CPU::LDEA,      4},
        {"LD H,B",     &CPU::LDHB,     4}, {"LD H,C",     &CPU::LDHC,      4}, {"LD H,D",     &CPU::LDHD,      4},
        {"LD H,E",     &CPU::LDHE,     4}, {"LD H,H",     &CPU::LDHH,      4}, {"LD H,L",     &CPU::LDHL,      4},
        {"LD H,(HL)",  &CPU::LDHmHL,   8}, {"LD H,A",     &CPU::LDHA,      4}, {"LD L,B",     &CPU::LDLB,      4},
        {"LD L,C",     &CPU::LDLC,     4}, {"LD L,D",     &CPU::LDLD,      4}, {"LD L,E",     &CPU::LDLE,      4},
        {"LD L,H",     &CPU::LDLH,     4}, {"LD L,L",     &CPU::LDLL,      4}, {"LD L,(HL)",  &CPU::LDLmHL,    8},
        {"LD L,A",     &CPU::LDLA,     4}, {"LD (HL),B",  &CPU::LDmHLB,    8}, {"LD (HL),C",  &CPU::LDmHLC,    8},
        {"LD (HL),D",  &CPU::LDmHLD,   8}, {"LD (HL),E",  &CPU::LDmHLE,    8}, {"LD (HL),H",  &CPU::LDmHLH,    8},
        {"LD (HL),L",  &CPU::LDmHLL,   8}, {"HALT",       &CPU::HALT,      4}, {"LD (HL),A",  &CPU::LDmHLA,    8},
        {"LD A,B",     &CPU::LDAB,     4}, {"LD A,C",     &CPU::LDAC,      4}, {"LD A,D",     &CPU::LDAD,      4},
        {"LD A,E",     &CPU::LDAE,     4}, {"LD A,H",     &CPU::LDAH,      4}, {"LD A,L",     &CPU::LDAL,      4},
        {"LD A,(HL)",  &CPU::LDAmHL,   8}, {"LD A,A",     &CPU::LDAA,      4}, {"ADD A,B",    &CPU::ADDAB,     4},
        {"ADD A,C",    &CPU::ADDAC,    4}, {"ADD A,D",    &CPU::ADDAD,     4}, {"ADD A,E",    &CPU::ADDAE,     4},
        {"ADD A,H",    &CPU::ADDAH,    4}, {"ADD A,L",    &CPU::ADDAL,     4}, {"ADD A,(HL)", &CPU::ADDAmHL,   8},
        {"ADD A,A",    &CPU::ADDAA,    4}, {"ADC A,B",    &CPU::ADCAB,     4}, {"ADC A,C",    &CPU::ADCAC,     4},
        {"ADC A,D",    &CPU::ADCAD,    4}, {"ADC A,E",    &CPU::ADCAE,     4}, {"ADC A,H",    &CPU::ADCAH,     4},
        {"ADC A,L",    &CPU::ADCAL,    4}, {"ADC A,(HL)", &CPU::ADCAmHL,   8}, {"ADC A,A",    &CPU::ADCAA,     4},
        {"SUB B",      &CPU::SUBB,     4}, {"SUB C",      &CPU::SUBC,      4}, {"SUB D",      &CPU::SUBD,      4},
        {"SUB E",      &CPU::SUBE,     4}, {"SUB H",      &CPU::SUBH,      4}, {"SUB L",      &CPU::SUBL,      4},
        {"SUB (HL)",   &CPU::SUBmHL,   8}, {"SUB A",      &CPU::SUBA,      4}, {"SBC A,B",    &CPU::SBCAB,     4},
        {"SBC A,C",    &CPU::SBCAC,    4}, {"SBC A,D",    &CPU::SBCAD,     4}, {"SBC A,E",    &CPU::SBCAE,     4},
        {"SBC A,H",    &CPU::SBCAH,    4}, {"SBC A,L",    &CPU::SBCAL,     4}, {"SBC A,(HL)", &CPU::SBCAmHL,   8},
        {"SBC A,A",    &CPU::SBCAA,    4}, {"AND B",      &CPU::ANDB,      4}, {"AND C",      &CPU::ANDC,      4}, 
        {"AND D",      &CPU::ANDD,     4}, {"AND E",      &CPU::ANDE,      4}, {"AND H",      &CPU::ANDH,      4},
        {"AND L",      &CPU::ANDL,     4}, {"AND (HL)",   &CPU::ANDmHL,    8}, {"AND A",      &CPU::ANDA,      4},
        {"XOR B",      &CPU::XORB,     4}, {"XOR C",      &CPU::XORC,      4}, {"XOR D",      &CPU::XORD,      4},
        {"XOR E",      &CPU::XORE,     4}, {"XOR H",      &CPU::XORH,      4}, {"XOR L",      &CPU::XORL,      4},
        {"XOR (HL)",   &CPU::XORmHL,   8}, {"XOR A",      &CPU::XORA,      4}, {"OR B",       &CPU::ORB,       4},
        {"OR C",       &CPU::ORC,      4}, {"OR D",       &CPU::ORD,       4}, {"OR E",       &CPU::ORE,       4},
        {"OR H",       &CPU::ORH,      4}, {"OR L",       &CPU::ORL,       4}, {"OR (HL)",    &CPU::ORmHL,     8},
        {"OR A",       &CPU::ORA,      4}, {"CP B",       &CPU::CPB,       4}, {"CP C",       &CPU::CPC,       4},
        {"CP D",       &CPU::CPD,      4}, {"CP E",       &CPU::CPE,       4}, {"CP H",       &CPU::CPH,       4},
        {"CP L",       &CPU::CP_L,     4}, {"CP (HL)",    &CPU::CPmHL,     8}, {"CP A",       &CPU::CPA,       4},
        {"RETnz",      &CPU::RETnz,    8}, {"POP BC",     &CPU::POPBC,    12}, {"JPnz,nn",    &CPU::JPnznn,   12},
        {"JPnn",       &CPU::JPnn,    16}, {"CALLnz,nn",  &CPU::CALLnznn, 12}, {"PUSH BC",    &CPU::PUSHBC,   16},
        {"ADD A,n",    &CPU::ADDAn,    8}, {"RST 00H",    &CPU::RST00H,   16}, {"RETz",       &CPU::RETz,      8},
        {"RET",        &CPU::RET,     16}, {"JPz,nn",     &CPU::JPznn,    12}, {"",           &CPU::CBn,       4},
        {"CALLz,nn",   &CPU::CALLznn, 12}, {"CALL nn",    &CPU::CALLnn,   24}, {"ADC A,n",    &CPU::ADCAn,     8},
        {"RST 08H",    &CPU::RST08H,  16}, {"RETnc",      &CPU::RETnc,     8}, {"POP DE",     &CPU::POPDE,    12},
        {"JPnc,nn",    &CPU::JPncnn,  12}, {"PANIC",      &CPU::PANIC,     0}, {"CALLnc,nn",  &CPU::CALLncnn, 12},
        {"PUSH DE",    &CPU::PUSHDE,  16}, {"SUB n",      &CPU::SUBAn,     8}, {"RST 10H",    &CPU::RST10H,   16},
        {"RETc",       &CPU::RETc,     8}, {"RETI",       &CPU::RETI,     16}, {"JPc,nn",     &CPU::JPcnn,    12},
        {"PANIC",      &CPU::PANIC,    0}, {"CALLc,nn",   &CPU::CALLcnn,  12}, {"PANIC",      &CPU::PANIC,     0},
        {"SBC A,n",    &CPU::SBCAn,    8}, {"RST 18H",    &CPU::RST18H,   16}, {"LDH (n),A",  &CPU::LDHmnA,   12},
        {"POP HL",     &CPU::POPHL,   12}, {"LD (C),A",   &CPU::LDmCA,     8}, {"PANIC",      &CPU::PANIC,     0},
        {"PANIC",      &CPU::PANIC,    0}, {"PUSH HL",    &CPU::PUSHHL,   16}, {"AND n",      &CPU::ANDn,      8},
        {"RST 20H",    &CPU::RST20H,  16}, {"ADD SP,n",   &CPU::ADDSPn,   16}, {"JP HL",      &CPU::JPHL,      4},
        {"LD (nn),A",  &CPU::LDmnnA,  16}, {"PANIC",      &CPU::PANIC,     0}, {"PANIC",      &CPU::PANIC,     0},
        {"PANIC",      &CPU::PANIC,    0}, {"XOR n",      &CPU::XORn,      8}, {"RST 28H",    &CPU::RST28H,   16},
        {"LDH A,(n)",  &CPU::LDHAmn,  12}, {"POP AF",     &CPU::POPAF,    12}, {"LD A,(C)",   &CPU::LDAmC,     8},
        {"DI",         &CPU::DI,       4}, {"PANIC",      &CPU::PANIC,     0}, {"PUSH AF",    &CPU::PUSHAF,   16},
        {"OR n",       &CPU::ORn,      8}, {"RST 30H",    &CPU::RST30H,   16}, {"LD HL,SP+n", &CPU::LDHLSPn,  12},
        {"LD SP,HL",   &CPU::LDSPHL,   8}, {"LD A,(nn)",  &CPU::LDAmnn,   16}, {"EI",         &CPU::EI,        4},
        {"PANIC",      &CPU::PANIC,    0}, {"PANIC",      &CPU::PANIC,     0}, {"CP n",       &CPU::CPn,       8},
        {"RST 38H",    &CPU::RST38H,  16}
    }};
    m_cbRom = {{
        {"RLC B",      &CPU::RLCB,     8}, {"RLC C",      &CPU::RLCC,     8}, {"RLC D", &CPU::RLCD,          8},
        {"RLC E",      &CPU::RLCE,     8}, {"RLC H",      &CPU::RLCH,     8}, {"RLC L", &CPU::RLCL,          8},
        {"RLC (HL)",   &CPU::RLCmHL,  16}, {"RLC A",      &CPU::RLC_A,    8}, {"RRC B", &CPU::RRCB,          8},
        {"RRC C",      &CPU::RRCC,     8}, {"RRC D",      &CPU::RRCD,     8}, {"RRC E", &CPU::RRCE,          8},
        {"RRC H",      &CPU::RRCH,     8}, {"RRC L",      &CPU::RRCL,     8}, {"RRC (HL)", &CPU::RRCmHL,    16},
        {"RRC A",      &CPU::RRC_A,    8}, {"RL B",       &CPU::RLB,      8}, {"RL C", &CPU::RLC,            8},
        {"RL D",       &CPU::RLD,      8}, {"RL E",       &CPU::RLE,      8}, {"RL H", &CPU::RLH,            8},
        {"RL L",       &CPU::RLL,      8}, {"RL (HL)",    &CPU::RLmHL,   16}, {"RL A", &CPU::RL_A,           8},
        {"RR B",       &CPU::RRB,      8}, {"RR C",       &CPU::RRC,      8}, {"RR D", &CPU::RRD,            8},
        {"RR E",       &CPU::RRE,      8}, {"RR H",       &CPU::RRH,      8}, {"RR L", &CPU::RRL,            8},
        {"RR (HL)",    &CPU::RRmHL,   16}, {"RR A",       &CPU::RR_A,     8}, {"SLA B", &CPU::SLAB,          8},
        {"SLA C",      &CPU::SLAC,     8}, {"SLA D",      &CPU::SLAD,     8}, {"SLA E", &CPU::SLAE,          8},
        {"SLA H",      &CPU::SLAH,     8}, {"SLA L",      &CPU::SLAL,     8}, {"SLA (HL)", &CPU::SLAmHL,    16},
        {"SLA A",      &CPU::SLAA,     8}, {"SRA B",      &CPU::SRAB,     8}, {"SRA C", &CPU::SRAC,          8},
        {"SRA D",      &CPU::SRAD,     8}, {"SRA E",      &CPU::SRAE,     8}, {"SRA H", &CPU::SRAH,          8},
        {"SRA L",      &CPU::SRAL,     8}, {"SRA (HL)",   &CPU::SRAmHL,  16}, {"SRA A", &CPU::SRAA,          8},
        {"SWAP B",     &CPU::SWAPB,    8}, {"SWAP C",     &CPU::SWAPC,    8}, {"SWAP D", &CPU::SWAPD,        8},
        {"SWAP E",     &CPU::SWAPE,    8}, {"SWAP H",     &CPU::SWAPH,    8}, {"SWAP L", &CPU::SWAPL,        8},
        {"SWAP (HL)",  &CPU::SWAPmHL, 16}, {"SWAP A",     &CPU::SWAPA,    8}, {"SRL B", &CPU::SRLB,          8},
        {"SRL C",      &CPU::SRLC,     8}, {"SRL D",      &CPU::SRLD,     8}, {"SRL E", &CPU::SRLE,          8},
        {"SRL H",      &CPU::SRLH,     8}, {"SRL L",      &CPU::SRLL,     8}, {"SRL (HL)", &CPU::SRLmHL,    16},
        {"SRL A",      &CPU::SRLA,     8}, {"BIT 0,B",    &CPU::BIT0B,    8}, {"BIT 0,C", &CPU::BIT0C,       8},
        {"BIT 0,D",    &CPU::BIT0D,    8}, {"BIT 0,E",    &CPU::BIT0E,    8}, {"BIT 0,H", &CPU::BIT0H,       8},
        {"BIT 0,L",    &CPU::BIT0L,    8}, {"BIT 0,(HL)", &CPU::BIT0mHL, 16}, {"BIT 0,A", &CPU::BIT0A,       8},
        {"BIT 1,B",    &CPU::BIT1B,    8}, {"BIT 1,C",    &CPU::BIT1C,    8}, {"BIT 1,D", &CPU::BIT1D,       8},
        {"BIT 1,E",    &CPU::BIT1E,    8}, {"BIT 1,H",    &CPU::BIT1H,    8}, {"BIT 1,L", &CPU::BIT1L,       8},
        {"BIT 1,(HL)", &CPU::BIT1mHL, 16}, {"BIT 1,A",    &CPU::BIT1A,    8}, {"BIT 2,B", &CPU::BIT2B,       8},
        {"BIT 2,C",    &CPU::BIT2C,    8}, {"BIT 2,D",    &CPU::BIT2D,    8}, {"BIT 2,E", &CPU::BIT2E,       8},
        {"BIT 2,H",    &CPU::BIT2H,    8}, {"BIT 2,L",    &CPU::BIT2L,    8}, {"BIT 2,(HL)", &CPU::BIT2mHL, 16},
        {"BIT 2,A",    &CPU::BIT2A,    8}, {"BIT 3,B",    &CPU::BIT3B,    8}, {"BIT 3,C", &CPU::BIT3C,       8},
        {"BIT 3,D",    &CPU::BIT3D,    8}, {"BIT 3,E",    &CPU::BIT3E,    8}, {"BIT 3,H", &CPU::BIT3H,       8},
        {"BIT 3,L",    &CPU::BIT3L,    8}, {"BIT 3,(HL)", &CPU::BIT3mHL, 16}, {"BIT 3,A", &CPU::BIT3A,       8},
        {"BIT 4,B",    &CPU::BIT4B,    8}, {"BIT 4,C",    &CPU::BIT4C,    8}, {"BIT 4,D", &CPU::BIT4D,       8},
        {"BIT 4,E",    &CPU::BIT4E,    8}, {"BIT 4,H",    &CPU::BIT4H,    8}, {"BIT 4,L", &CPU::BIT4L,       8},
        {"BIT 4,(HL)", &CPU::BIT4mHL, 16}, {"BIT 4,A",    &CPU::BIT4A,    8}, {"BIT 5,B", &CPU::BIT5B,       8},
        {"BIT 5,C",    &CPU::BIT5C,    8}, {"BIT 5,D",    &CPU::BIT5D,    8}, {"BIT 5,E", &CPU::BIT5E,       8},
        {"BIT 5,H",    &CPU::BIT5H,    8}, {"BIT 5,L",    &CPU::BIT5L,    8}, {"BIT 5,(HL)", &CPU::BIT5mHL, 16},
        {"BIT 5,A",    &CPU::BIT5A,    8}, {"BIT 6,B",    &CPU::BIT6B,    8}, {"BIT 6,C", &CPU::BIT6C,       8},
        {"BIT 6,D",    &CPU::BIT6D,    8}, {"BIT 6,E",    &CPU::BIT6E,    8}, {"BIT 6,H", &CPU::BIT6H,       8},
        {"BIT 6,L",    &CPU::BIT6L,    8}, {"BIT 6,(HL)", &CPU::BIT6mHL, 16}, {"BIT 6,A", &CPU::BIT6A,       8},
        {"BIT 7,B",    &CPU::BIT7B,    8}, {"BIT 7,C",    &CPU::BIT7C,    8}, {"BIT 7,D", &CPU::BIT7D,       8},
        {"BIT 7,E",    &CPU::BIT7E,    8}, {"BIT 7,H",    &CPU::BIT7H,    8}, {"BIT 7,L", &CPU::BIT7L,       8},
        {"BIT 7,(HL)", &CPU::BIT7mHL, 16}, {"BIT 7,A",    &CPU::BIT7A,    8}, {"RES 0,B", &CPU::RES0B,       8},
        {"RES 0,C",    &CPU::RES0C,    8}, {"RES 0,D",    &CPU::RES0D,    8}, {"RES 0,E", &CPU::RES0E,       8},
        {"RES 0,H",    &CPU::RES0H,    8}, {"RES 0,L",    &CPU::RES0L,    8}, {"RES 0,(HL)", &CPU::RES0mHL, 16},
        {"RES 0,A",    &CPU::RES0A,    8}, {"RES 1,B",    &CPU::RES1B,    8}, {"RES 1,C", &CPU::RES1C,       8},
        {"RES 1,D",    &CPU::RES1D,    8}, {"RES 1,E",    &CPU::RES1E,    8}, {"RES 1,H", &CPU::RES1H,       8},
        {"RES 1,L",    &CPU::RES1L,    8}, {"RES 1,(HL)", &CPU::RES1mHL, 16}, {"RES 1,A", &CPU::RES1A,       8},
        {"RES 2,B",    &CPU::RES2B,    8}, {"RES 2,C",    &CPU::RES2C,    8}, {"RES 2,D", &CPU::RES2D,       8},
        {"RES 2,E",    &CPU::RES2E,    8}, {"RES 2,H",    &CPU::RES2H,    8}, {"RES 2,L", &CPU::RES2L,       8},
        {"RES 2,(HL)", &CPU::RES2mHL, 16}, {"RES 2,A",    &CPU::RES2A,    8}, {"RES 3,B", &CPU::RES3B,       8},
        {"RES 3,C",    &CPU::RES3C,    8}, {"RES 3,D",    &CPU::RES3D,    8}, {"RES 3,E", &CPU::RES3E,       8},
        {"RES 3,H",    &CPU::RES3H,    8}, {"RES 3,L",    &CPU::RES3L,    8}, {"RES 3,(HL)", &CPU::RES3mHL, 16},
        {"RES 3,A",    &CPU::RES3A,    8}, {"RES 4,B",    &CPU::RES4B,    8}, {"RES 4,C", &CPU::RES4C,       8},
        {"RES 4,D",    &CPU::RES4D,    8}, {"RES 4,E",    &CPU::RES4E,    8}, {"RES 4,H", &CPU::RES4H,       8},
        {"RES 4,L",    &CPU::RES4L,    8}, {"RES 4,(HL)", &CPU::RES4mHL, 16}, {"RES 4,A", &CPU::RES4A,       8},
        {"RES 5,B",    &CPU::RES5B,    8}, {"RES 5,C",    &CPU::RES5C,    8}, {"RES 5,D", &CPU::RES5D,       8},
        {"RES 5,E",    &CPU::RES5E,    8}, {"RES 5,H",    &CPU::RES5H,    8}, {"RES 5,L", &CPU::RES5L,       8},
        {"RES 5,(HL)", &CPU::RES5mHL, 16}, {"RES 5,A",    &CPU::RES5A,    8}, {"RES 6,B", &CPU::RES6B,       8},
        {"RES 6,C",    &CPU::RES6C,    8}, {"RES 6,D",    &CPU::RES6D,    8}, {"RES 6,E", &CPU::RES6E,       8},
        {"RES 6,H",    &CPU::RES6H,    8}, {"RES 6,L",    &CPU::RES6L,    8}, {"RES 6,(HL)", &CPU::RES6mHL, 16},
        {"RES 6,A",    &CPU::RES6A,    8}, {"RES 7,B",    &CPU::RES7B,    8}, {"RES 7,C", &CPU::RES7C,       8},
        {"RES 7,D",    &CPU::RES7D,    8}, {"RES 7,E",    &CPU::RES7E,    8}, {"RES 7,H", &CPU::RES7H,       8},
        {"RES 7,L",    &CPU::RES7L,    8}, {"RES 7,(HL)", &CPU::RES7mHL, 16}, {"RES 7,A", &CPU::RES7A,       8},
        {"SET 0,B",    &CPU::SET0B,    8}, {"SET 0,C",    &CPU::SET0C,    8}, {"SET 0,D", &CPU::SET0D,       8},
        {"SET 0,E",    &CPU::SET0E,    8}, {"SET 0,H",    &CPU::SET0H,    8}, {"SET 0,L", &CPU::SET0L,       8},
        {"SET 0,(HL)", &CPU::SET0mHL, 16}, {"SET 0,A",    &CPU::SET0A,    8}, {"SET 1,B", &CPU::SET1B,       8},
        {"SET 1,C",    &CPU::SET1C,    8}, {"SET 1,D",    &CPU::SET1D,    8}, {"SET 1,E", &CPU::SET1E,       8},
        {"SET 1,H",    &CPU::SET1H,    8}, {"SET 1,L",    &CPU::SET1L,    8}, {"SET 1,(HL)", &CPU::SET1mHL, 16},
        {"SET 1,A",    &CPU::SET1A,    8}, {"SET 2,B",    &CPU::SET2B,    8}, {"SET 2,C", &CPU::SET2C,       8},
        {"SET 2,D",    &CPU::SET2D,    8}, {"SET 2,E",    &CPU::SET2E,    8}, {"SET 2,H", &CPU::SET2H,       8},
        {"SET 2,L",    &CPU::SET2L,    8}, {"SET 2,(HL)", &CPU::SET2mHL, 16}, {"SET 2,A", &CPU::SET2A,       8},
        {"SET 3,B",    &CPU::SET3B,    8}, {"SET 3,C",    &CPU::SET3C,    8}, {"SET 3,D", &CPU::SET3D,       8},
        {"SET 3,E",    &CPU::SET3E,    8}, {"SET 3,H",    &CPU::SET3H,    8}, {"SET 3,L", &CPU::SET3L,       8},
        {"SET 3,(HL)", &CPU::SET3mHL, 16}, {"SET 3,A",    &CPU::SET3A,    8}, {"SET 4,B", &CPU::SET4B,       8},
        {"SET 4,C",    &CPU::SET4C,    8}, {"SET 4,D",    &CPU::SET4D,    8}, {"SET 4,E", &CPU::SET4E,       8},
        {"SET 4,H",    &CPU::SET4H,    8}, {"SET 4,L",    &CPU::SET4L,    8}, {"SET 4,(HL)", &CPU::SET4mHL, 16},
        {"SET 4,A",    &CPU::SET4A,    8}, {"SET 5,B",    &CPU::SET5B,    8}, {"SET 5,C", &CPU::SET5C,       8},
        {"SET 5,D",    &CPU::SET5D,    8}, {"SET 5,E",    &CPU::SET5E,    8}, {"SET 5,H", &CPU::SET5H,       8},
        {"SET 5,L",    &CPU::SET5L,    8}, {"SET 5,(HL)", &CPU::SET5mHL, 16}, {"SET 5,A", &CPU::SET5A,       8},
        {"SET 6,B",    &CPU::SET6B,    8}, {"SET 6,C",    &CPU::SET6C,    8}, {"SET 6,D", &CPU::SET6D,       8},
        {"SET 6,E",    &CPU::SET6E,    8}, {"SET 6,H",    &CPU::SET6H,    8}, {"SET 6,L", &CPU::SET6L,       8},
        {"SET 6,(HL)", &CPU::SET6mHL, 16}, {"SET 6,A",    &CPU::SET6A,    8}, {"SET 7,B", &CPU::SET7B,       8},
        {"SET 7,C",    &CPU::SET7C,    8}, {"SET 7,D",    &CPU::SET7D,    8}, {"SET 7,E", &CPU::SET7E,       8},
        {"SET 7,H",    &CPU::SET7H,    8}, {"SET 7,L",    &CPU::SET7L,    8}, {"SET 7,(HL)", &CPU::SET7mHL,  8},
        {"SET 7,A",    &CPU::SET7A,    8}
    }};
}

CPU::~CPU() = default;

void CPU::connectGameBoy(GameBoy* gb) {
    m_gb = gb;
    m_timer.connectGameBoy(m_gb);
}

/**
 * @brief Operations to perform upon receiving a reset signal
 */
void CPU::reset() {
    m_a = 0x00;
    m_b = 0x00;
    m_c = 0x00;
    m_d = 0x00;
    m_e = 0x00;
    m_f = 0x00;
    m_h = 0x00;
    m_l = 0x00;
    m_pc = 0x0100;
    m_sp = 0xFFFE;
    m_intMaster = true;
    m_intFlags = 0x00;
    m_intEnable = 0x00;
    m_stop = false;
    m_halt = false;
    m_cycles = 0;
    m_timer.reset();
}

/**
 * Handle the next instruction.
 *
 * First the next opcode is fetched from memory, then this opcode is decoded and executed using a
 * jump table of function pointers which is indexed by the opcode. After this, the number of
 * cycles that instruction takes on the actual Zilog z80 CPU is returned; this value is used for
 * timing purposes.
 */
uint8_t CPU::step() {
    stepInterrupt();

    if (m_stop) {
        return 0x01;
    }
    if (m_halt) {
        return handleHalt();
    }

    m_opcode = fetch();
    m_cycles = m_instRom[m_opcode].cycles;
    (this->*m_instRom[m_opcode].op)();
    logInfo();
    return m_cycles;
}

uint8_t CPU::handleHalt() {
    if (m_intMaster) {
        if (m_intEnable & m_intFlags) {
            m_halt = false;
            push16(m_pc + 1);
            return 0x01;
        }

        m_opcode = 0x00; // simulate NOP
        m_cycles = m_instRom[m_opcode].cycles;
        (this->*m_instRom[m_opcode].op)();
        return m_cycles;
    }

    if (m_intEnable & m_intFlags) {
        // Halt bug
        uint16_t pc = m_pc;
        m_halt = false;
        m_opcode = fetch();
        m_cycles = m_instRom[m_opcode].cycles;
        (this->*m_instRom[m_opcode].op)();
        m_pc = pc;
        m_opcode = fetch();
        m_cycles = m_instRom[m_opcode].cycles;
        (this->*m_instRom[m_opcode].op)();
        return m_cycles;
    }

    m_opcode = 0x00;
    m_cycles = m_instRom[m_opcode].cycles;
    (this->*m_instRom[m_opcode].op)();
    return m_cycles;
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
inline uint8_t CPU::fetch() { return read8(m_pc++); }
inline void CPU::setBit(uint8_t f) { m_f |= f; }
inline void CPU::clrBit(uint8_t f) { m_f &= ~f; }

void CPU::stepInterrupt() {
    if (m_intMaster) {
        uint8_t on = m_intEnable & m_intFlags;

        if (on) {
            m_intMaster = false;
            push16(m_pc);
            m_gb->m_gpu.m_clocks += 12;

            if (on & INT_VBLANK) {
                m_intFlags &= ~INT_VBLANK;
                vblankISR();
            } else if (on & INT_LCDC) {
                m_intFlags &= ~INT_LCDC;
                lcdcISR();
            } else if (on & INT_TIMER) {
                m_intFlags &= ~INT_TIMER;
                timerISR();
            } else if (on & INT_SERIAL) {
                m_intFlags &= ~INT_SERIAL;
                serialISR();
            } else if (on & INT_HITOLO) {
                m_intFlags &= ~INT_HITOLO;
                hitoloISR();
            }
        }
    }
}

void CPU::vblankISR() { m_pc = PC_VBLANK; }
void CPU::lcdcISR() { m_pc = PC_LCDC; }
void CPU::timerISR() { m_pc = PC_TIMER; }
void CPU::serialISR() { m_pc = PC_SERIAL; }
void CPU::hitoloISR() { m_pc = PC_HITOLO; }
uint8_t CPU::read8(uint16_t addr) { return m_gb->read8(addr); }
uint16_t CPU::read16(uint16_t addr) { return m_gb->read16(addr); }
void CPU::write8(uint16_t addr, uint8_t data) { m_gb->write8(addr, data); }
void CPU::write16(uint16_t addr, uint16_t data) { m_gb->write16(addr, data); }

uint16_t CPU::pop16() {
    uint16_t r = read16(m_sp);
    m_sp += 2;
    return r;
}

void CPU::push16(uint16_t data) {
    m_sp -= 2;
    write16(m_sp, data);
}

void CPU::_add8(uint8_t operand) {
    uint16_t sum = m_a + operand;
    (((m_a & 0x0F) + (operand & 0x0F)) > 0x0F) ? setBit(HALF) : clrBit(HALF);
    sum & 0x0100 ? setBit(CARRY) : clrBit(CARRY);
    sum & 0x00FF ? clrBit(ZERO) : setBit(ZERO);
    clrBit(SIGN);
    m_a = sum & 0x00FF;
}

void CPU::_add16(uint16_t operand) {
    uint32_t sum = PAIR(m_h, m_l) + operand;
    sum & 0x10000 ? setBit(CARRY) : clrBit(CARRY);
    (((PAIR(m_h, m_l) & 0x0FFF) + (operand & 0x0FFF)) > 0x0FFF) ? setBit(HALF) : clrBit(HALF);
    clrBit(SIGN);
    m_h = (sum & 0xFF00) >> 8;
    m_l = sum & 0x00FF;
}

void CPU::_adc(uint8_t operand) {
    uint8_t carry = (m_f & CARRY) ? 1 : 0;
    uint16_t a = operand + m_a + carry;
    a > 0xFF ? setBit(CARRY) : clrBit(CARRY);
    (((m_a & 0x0F) + (operand & 0x0F) + carry) > 0x0F) ? setBit(HALF) : clrBit(HALF);
    m_a = a & 0x00FF;
    m_a ? clrBit(ZERO) : setBit(ZERO);
    clrBit(SIGN);
}

void CPU::_sub(uint8_t operand) {
    operand > m_a ? setBit(CARRY) : clrBit(CARRY);
    (operand & 0x0F) > (m_a & 0x0F) ? setBit(HALF) : clrBit(HALF);
    m_a -= operand;
    m_a ? clrBit(ZERO) : setBit(ZERO);
    setBit(SIGN);
}

void CPU::_sbc(uint8_t operand) {
    uint8_t carry = (m_f & CARRY) ? 1 : 0;
    int16_t diff = m_a - operand - carry;
    ((m_a & 0x0F) - (operand & 0x0F) - carry) < 0x00 ? setBit(HALF) : clrBit(HALF);
    diff < 0x00 ? setBit(CARRY) : clrBit(CARRY);
    m_a = diff & 0x00FF;
    m_a ? clrBit(ZERO) : setBit(ZERO);
    setBit(SIGN);
}

void CPU::_and(uint8_t operand) {
    m_a &= operand;
    m_a ? clrBit(ZERO) : setBit(ZERO);
    setBit(HALF);
    clrBit(SIGN | CARRY);
}

void CPU::_xor(uint8_t operand) {
    m_a ^= operand;
    m_a ? clrBit(ZERO) : setBit(ZERO);
    clrBit(SIGN | CARRY | HALF);
}

void CPU::_or(uint8_t operand) {
    m_a |= operand;
    m_a ? clrBit(ZERO) : setBit(ZERO);
    clrBit(SIGN | CARRY | HALF);
}

void CPU::_cp(uint8_t operand) {
    m_a == operand ? setBit(ZERO) : clrBit(ZERO);
    operand > m_a ? setBit(CARRY) : clrBit(CARRY);
    (operand & 0x0F) > (m_a & 0x0F) ? setBit(HALF) : clrBit(HALF);
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
    uint8_t carry = (m_f & CARRY) ? 1 : 0;
    operand & 0x80 ? setBit(CARRY) : clrBit(CARRY);
    operand = (operand << 1) | carry;
    operand ? clrBit(ZERO) : setBit(ZERO);
    clrBit(SIGN | HALF);
    return operand;
}

uint8_t CPU::_rr(uint8_t operand) {
    uint8_t carry = (m_f & CARRY) ? 0x80 : 0x00;
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

uint8_t CPU::_setBit(uint8_t mask, uint8_t operand) {
    return operand | mask;
}

void CPU::PANIC() {
    std::cout << "Unknown opcode 0x" << std::hex << static_cast<int>(m_opcode) << std::endl;
    m_pc--;
}

/*******************************************************************************
 *                              Instruction Set                                *
 ******************************************************************************/
void CPU::NOP() { }

void CPU::LDBCnn() {
    uint16_t bc = read16(m_pc);
    m_pc += 2;
    m_b = (bc & 0xFF00) >> 8;
    m_c = bc & 0x00FF;
}

void CPU::LDmBCA() { write8(PAIR(m_b, m_c), m_a); }

void CPU::INCBC() {
    m_c++;
    if (!m_c) {
        m_b++;
    }
}

void CPU::INCB() { m_b = _inc(m_b); }
void CPU::DECB() { m_b = _dec(m_b); }
void CPU::LDBn() { m_b = fetch(); }

void CPU::RLCA() {
    uint8_t carry = (m_a & 0x80) >> 7;
    carry ? setBit(CARRY) : clrBit(CARRY);
    m_a = (m_a << 1) | carry;
    clrBit(SIGN | HALF | ZERO);
}

void CPU::LDmnnSP() {
    write16(read16(m_pc), m_sp);
    m_pc += 2;
}

void CPU::ADDHLBC() { _add16(PAIR(m_b, m_c)); }
void CPU::LDAmBC() { m_a = read8(PAIR(m_b, m_c)); }

void CPU::DECBC() {
    if (!m_c) {
        m_b--;
    }
    m_c--;
}

void CPU::INCC() { m_c = _inc(m_c); }
void CPU::DECC() { m_c = _dec(m_c); }
void CPU::LDCn() { m_c = fetch(); }

void CPU::RRCA() {
    uint8_t carry = (m_a & 0x01) << 7;
    carry ? setBit(CARRY) : clrBit(CARRY);
    m_a = (m_a >> 1) | carry;
    clrBit(SIGN | HALF | ZERO);
}

void CPU::STOP() { m_stop = 1; }

void CPU::LDDEnn() {
    uint16_t de = read16(m_pc);
    m_pc += 2;
    m_d = (de & 0xFF00) >> 8;
    m_e = de & 0x00FF;
}

void CPU::LDmDEA() { write8(PAIR(m_d, m_e), m_a); }

void CPU::INCDE() {
    m_e++;
    if (!m_e) {
        m_d++;
    }
}

void CPU::INCD() { m_d = _inc(m_d); }
void CPU::DECD() { m_d = _dec(m_d); }
void CPU::LDDn() { m_d = fetch(); }

void CPU::RLA() {
    uint8_t carry = (m_f & CARRY) ? 1 : 0;
    m_a & 0x80 ? setBit(CARRY) : clrBit(CARRY);
    m_a = (m_a << 1) | carry;
    clrBit(SIGN | HALF | ZERO);
}

void CPU::JRn() { m_pc += static_cast<int8_t>(fetch()); }
void CPU::ADDHLDE() { _add16(PAIR(m_d, m_e)); }
void CPU::LDAmDE() { m_a = read8(PAIR(m_d, m_e)); }

void CPU::DECDE() {
    if (!m_e) {
        m_d--;
    }
    m_e--;
}

void CPU::INCE() { m_e = _inc(m_e); }
void CPU::DECE() { m_e = _dec(m_e); }
void CPU::LDEn() { m_e = fetch(); }

void CPU::RRA() {
    uint8_t carry = (m_f & CARRY) ? 0x80 : 0x00;
    m_a & 0x01 ? setBit(CARRY) : clrBit(CARRY);
    m_a = (m_a >> 1) | carry;
    clrBit(HALF | SIGN | ZERO);
}

void CPU::JRnzn() {
    int8_t offset = static_cast<int8_t>(fetch());
    if (!(m_f & ZERO)) {
        m_pc += offset;
        m_cycles += 4;
    }
}

void CPU::LDHLnn() {
    uint16_t hl = read16(m_pc);
    m_pc += 2;
    m_h = (hl & 0xFF00) >> 8;
    m_l = hl & 0x00FF;
}

void CPU::LDmHLpA() {
    write8(PAIR(m_h, m_l), m_a);
    m_l++;
    if (!m_l) {
        m_h++;
    }
}

void CPU::INCHL() {
    m_l++;
    if (!m_l) {
        m_h++;
    }
}

void CPU::INCH() { m_h = _inc(m_h); }
void CPU::DECH() { m_h = _dec(m_h); }
void CPU::LDHn() { m_h = fetch(); }

void CPU::DAA() {
    int32_t a = m_a;

    if (!(m_f & SIGN)) {
        if ((m_f & HALF) || (a & 0x0F) > 9) {
            a += 0x06;
        }
        if ((m_f & CARRY) || a > 0x9F) {
            a += 0x60;
        }
    } else {
        if (m_f & HALF) {
            a = (a - 6) & 0xFF;
        }
        if (m_f & CARRY) {
            a -= 0x60;
        }
    }
    if ((a & 0x0100) == 0x0100) {
        setBit(CARRY);
    }
    a &= 0xFF;
    a ? clrBit(ZERO) : setBit(ZERO);
    clrBit(HALF);
    m_a = static_cast<uint8_t>(a);
}

void CPU::JRzn() {
    auto offset = static_cast<int8_t>(fetch());
    if (m_f & ZERO) {
        m_pc += offset;
        m_cycles += 4;
    }
}

void CPU::ADDHLHL() { _add16(PAIR(m_h, m_l)); }

void CPU::LDAmHLp() {
    m_a = read8(PAIR(m_h, m_l));
    m_l++;
    if (!m_l) {
        m_h++;
    }
}

void CPU::DECHL() {
    if (!m_l) {
        m_h--;
    }
    m_l--;
}

void CPU::INCL() { m_l = _inc(m_l); }
void CPU::DECL() { m_l = _dec(m_l); }
void CPU::LDLn() { m_l = fetch(); }

void CPU::CPL() {
    m_a = ~m_a;
    setBit(SIGN | HALF);
}

void CPU::JRncn() {
    auto offset = static_cast<int8_t>(fetch());
    if (!(m_f & CARRY)) {
        m_pc += offset;
        m_cycles += 4;
    }
}

void CPU::LDSPnn() {
    m_sp = read16(m_pc);
    m_pc += 2;
}

void CPU::LDmHLmA() {
    write8(PAIR(m_h, m_l), m_a);
    if (!m_l) {
        m_h--;
    }
    m_l--;
}

void CPU::INCSP() { m_sp++; }
void CPU::INCmHL() { write8(PAIR(m_h, m_l), _inc(read8(PAIR(m_h, m_l)))); }
void CPU::DECmHL() { write8(PAIR(m_h, m_l), _dec(read8(PAIR(m_h, m_l)))); }
void CPU::LDmHLn() { write8(PAIR(m_h, m_l), fetch()); }

void CPU::SCF() {
    setBit(CARRY);
    clrBit(SIGN | HALF);
}

void CPU::JRcn() {
    auto offset = static_cast<int8_t>(fetch());
    if (m_f & CARRY) {
        m_pc += offset;
        m_cycles += 4;
    }
}

void CPU::ADDHLSP() { _add16(m_sp); }
void CPU::LDAmHLm() {
    m_a = read8(PAIR(m_h, m_l));
    if (!m_l) {
        m_h--;
    }
    m_l--;
}

void CPU::DECSP() { m_sp--; }
void CPU::INCA() { m_a = _inc(m_a); }
void CPU::DECA() { m_a = _dec(m_a); }
void CPU::LDAn() { m_a = fetch(); }

void CPU::CCF() {
    (m_f & CARRY) ? clrBit(CARRY) : setBit(CARRY);
    clrBit(SIGN | HALF);
}

void CPU::LDBB() { }
void CPU::LDBC() { m_b = m_c; }
void CPU::LDBD() { m_b = m_d; }
void CPU::LDBE() { m_b = m_e; }
void CPU::LDBH() { m_b = m_h; }
void CPU::LDBL() { m_b = m_l; }
void CPU::LDBmHL() { m_b = read8(PAIR(m_h, m_l)); }
void CPU::LDBA() { m_b = m_a; }
void CPU::LDCB() { m_c = m_b; }
void CPU::LDCC() { }
void CPU::LDCD() { m_c = m_d; }
void CPU::LDCE() { m_c = m_e; }
void CPU::LDCH() { m_c = m_h; }
void CPU::LDCL() { m_c = m_l; }
void CPU::LDCmHL() { m_c = read8(PAIR(m_h, m_l)); }
void CPU::LDCA() { m_c = m_a; }
void CPU::LDDB() { m_d = m_b; }
void CPU::LDDC() { m_d = m_c; }
void CPU::LDDD() { }
void CPU::LDDE() { m_d = m_e; }
void CPU::LDDH() { m_d = m_h; }
void CPU::LDDL() { m_d = m_l; }
void CPU::LDDmHL() { m_d = read8(PAIR(m_h, m_l)); }
void CPU::LDDA() { m_d = m_a; }
void CPU::LDEB() { m_e = m_b; }
void CPU::LDEC() { m_e = m_c; }
void CPU::LDED() { m_e = m_d; }
void CPU::LDEE() { }
void CPU::LDEH() { m_e = m_h; }
void CPU::LDEL() { m_e = m_l; }
void CPU::LDEmHL() { m_e = read8(PAIR(m_h, m_l)); }
void CPU::LDEA() { m_e = m_a; }
void CPU::LDHB() { m_h = m_b; }
void CPU::LDHC() { m_h = m_c; }
void CPU::LDHD() { m_h = m_d; }
void CPU::LDHE() { m_h = m_e; }
void CPU::LDHH() { }
void CPU::LDHL() { m_h = m_l; }
void CPU::LDHmHL() { m_h = read8(PAIR(m_h, m_l)); }
void CPU::LDHA() { m_h = m_a; }
void CPU::LDLB() { m_l = m_b; }
void CPU::LDLC() { m_l = m_c; }
void CPU::LDLD() { m_l = m_d; }
void CPU::LDLE() { m_l = m_e; }
void CPU::LDLH() { m_l = m_h; }
void CPU::LDLL() { }
void CPU::LDLmHL() { m_l = read8(PAIR(m_h, m_l)); }
void CPU::LDLA() { m_l = m_a; }
void CPU::LDmHLB() { write8(PAIR(m_h, m_l), m_b); }
void CPU::LDmHLC() { write8(PAIR(m_h, m_l), m_c); }
void CPU::LDmHLD() { write8(PAIR(m_h, m_l), m_d); }
void CPU::LDmHLE() { write8(PAIR(m_h, m_l), m_e); }
void CPU::LDmHLH() { write8(PAIR(m_h, m_l), m_h); }
void CPU::LDmHLL() { write8(PAIR(m_h, m_l), m_l); }
void CPU::HALT() { m_halt = 1; }
void CPU::LDmHLA() { write8(PAIR(m_h, m_l), m_a); }
void CPU::LDAB() { m_a = m_b; }
void CPU::LDAC() { m_a = m_c; }
void CPU::LDAD() { m_a = m_d; }
void CPU::LDAE() { m_a = m_e; }
void CPU::LDAH() { m_a = m_h; }
void CPU::LDAL() { m_a = m_l; }
void CPU::LDAmHL() { m_a = read8(PAIR(m_h, m_l)); }
void CPU::LDAA() { }
void CPU::ADDAB() { _add8(m_b); }
void CPU::ADDAC() { _add8(m_c); }
void CPU::ADDAD() { _add8(m_d); }
void CPU::ADDAE() { _add8(m_e); }
void CPU::ADDAH() { _add8(m_h); }
void CPU::ADDAL() { _add8(m_l); }
void CPU::ADDAmHL() { _add8(read8(PAIR(m_h, m_l))); }
void CPU::ADDAA() { _add8(m_a); }
void CPU::ADCAB() { _adc(m_b); }
void CPU::ADCAC() { _adc(m_c); }
void CPU::ADCAD() { _adc(m_d); }
void CPU::ADCAE() { _adc(m_e); }
void CPU::ADCAH() { _adc(m_h); }
void CPU::ADCAL() { _adc(m_l); }
void CPU::ADCAmHL() { _adc(read8(PAIR(m_h, m_l))); }
void CPU::ADCAA() { _adc(m_a); }
void CPU::SUBB() { _sub(m_b); }
void CPU::SUBC() { _sub(m_c); }
void CPU::SUBD() { _sub(m_d); }
void CPU::SUBE() { _sub(m_e); }
void CPU::SUBH() { _sub(m_h); }
void CPU::SUBL() { _sub(m_l); }
void CPU::SUBmHL() { _sub(read8(PAIR(m_h, m_l))); }
void CPU::SUBA() { _sub(m_a); }
void CPU::SBCAB() { _sbc(m_b); }
void CPU::SBCAC() { _sbc(m_c); }
void CPU::SBCAD() { _sbc(m_d); }
void CPU::SBCAE() { _sbc(m_e); }
void CPU::SBCAH() { _sbc(m_h); }
void CPU::SBCAL() { _sbc(m_l); }
void CPU::SBCAmHL() { _sbc(read8(PAIR(m_h, m_l))); }
void CPU::SBCAA() { _sbc(m_a); }
void CPU::ANDB() { _and(m_b); }
void CPU::ANDC() { _and(m_c); }
void CPU::ANDD() { _and(m_d); }
void CPU::ANDE() { _and(m_e); }
void CPU::ANDH() { _and(m_h); }
void CPU::ANDL() { _and(m_l); }
void CPU::ANDmHL() { _and(read8(PAIR(m_h, m_l))); }
void CPU::ANDA() { _and(m_a); }
void CPU::XORB() { _xor(m_b); }
void CPU::XORC() { _xor(m_c); }
void CPU::XORD() { _xor(m_d); }
void CPU::XORE() { _xor(m_e); }
void CPU::XORH() { _xor(m_h); }
void CPU::XORL() { _xor(m_l); }
void CPU::XORmHL() { _xor(read8(PAIR(m_h, m_l))); }
void CPU::XORA() { _xor(m_a); }
void CPU::ORB() { _or(m_b); }
void CPU::ORC() { _or(m_c); }
void CPU::ORD() { _or(m_d); }
void CPU::ORE() { _or(m_e); }
void CPU::ORH() { _or(m_h); }
void CPU::ORL() { _or(m_l); }
void CPU::ORmHL() { _or(read8(PAIR(m_h, m_l))); }
void CPU::ORA() { _or(m_a); }
void CPU::CPB() { _cp(m_b); }
void CPU::CPC() { _cp(m_c); }
void CPU::CPD() { _cp(m_d); }
void CPU::CPE() { _cp(m_e); }
void CPU::CPH() { _cp(m_h); }
void CPU::CP_L() { _cp(m_l); }
void CPU::CPmHL() { _cp(read8(PAIR(m_h, m_l))); }
void CPU::CPA() { _cp(m_a); }

void CPU::RETnz() {
    if (!(m_f & ZERO)) {
        m_pc = pop16();
        m_cycles += 12;
    }
}

void CPU::POPBC() {
    uint16_t bc = pop16();
    m_b = (bc & 0xFF00) >> 8;
    m_c = bc & 0x00FF;
}

void CPU::JPnznn() {
    uint16_t operand = read16(m_pc);
    m_pc += 2;
    if (!(m_f & ZERO)) {
        m_pc = operand;
        m_cycles += 4;
    }
}

void CPU::JPnn() { m_pc = read16(m_pc); }

void CPU::CALLnznn() {
    uint16_t operand = read16(m_pc);
    m_pc += 2;
    if (!(m_f & ZERO)) {
        push16(m_pc);
        m_pc = operand;
        m_cycles += 12;
    }
}

void CPU::PUSHBC() { push16(PAIR(m_b, m_c)); }
void CPU::ADDAn() { _add8(fetch()); }

void CPU::RST00H() {
    push16(m_pc);
    m_pc = 0x0000;
}

void CPU::RETz() {
    if (m_f & ZERO) {
        m_pc = pop16();
        m_cycles += 12;
    }
}

void CPU::RET() { m_pc = pop16(); }

void CPU::JPznn() {
    uint16_t operand = read16(m_pc);
    m_pc += 2;
    if (m_f & ZERO) {
        m_pc = operand;
        m_cycles += 4;
    }
}

void CPU::CBn() {
    m_opcode = fetch();
    (*this.*m_cbRom[m_opcode].op)();
    m_cycles += m_cbRom[m_opcode].cycles;
}

void CPU::CALLznn() {
    uint16_t operand = read16(m_pc);
    m_pc += 2;
    if (m_f & ZERO) {
        push16(m_pc);
        m_pc = operand;
        m_cycles += 12;
    }
}

void CPU::CALLnn() {
    uint16_t operand = read16(m_pc);
    m_pc += 2;
    push16(m_pc);
    m_pc = operand;
}

void CPU::ADCAn() { _adc(fetch()); }

void CPU::RST08H() {
    push16(m_pc);
    m_pc = 0x0008;
}

void CPU::RETnc() {
    if (!(m_f & CARRY)) {
        m_pc = pop16();
        m_cycles += 12;
    }
}

void CPU::POPDE() {
    uint16_t de = pop16();
    m_d = (de & 0xFF00) >> 8;
    m_e = de & 0x00FF;
}

void CPU::JPncnn() {
    uint16_t operand = read16(m_pc);
    m_pc += 2;
    if (!(m_f & CARRY)) {
        m_pc = operand;
        m_cycles += 4;
    }
}

void CPU::CALLncnn() {
    uint16_t operand = read16(m_pc);
    m_pc += 2;
    if (!(m_f & CARRY)) {
        push16(m_pc);
        m_pc = operand;
        m_cycles += 12;
    }
}

void CPU::PUSHDE() { push16(PAIR(m_d, m_e)); }
void CPU::SUBAn() { _sub(fetch()); }

void CPU::RST10H() {
    push16(m_pc);
    m_pc = 0x0010;
}

void CPU::RETc() {
    if (m_f & CARRY) {
        m_pc = pop16();
        m_cycles += 12;
    }
}

void CPU::RETI() {
    m_intMaster = true;
    m_pc = pop16();
}

void CPU::JPcnn() {
    uint16_t operand = read16(m_pc);
    m_pc += 2;
    if (m_f & CARRY) {
        m_pc = operand;
        m_cycles += 4;
    }
}

void CPU::CALLcnn() {
    uint16_t operand = read16(m_pc);
    m_pc += 2;
    if (m_f & CARRY) {
        push16(m_pc);
        m_pc = operand;
        m_cycles += 12;
    }
}

void CPU::SBCAn() { _sbc(fetch()); }

void CPU::RST18H() {
    push16(m_pc);
    m_pc = 0x0018;
}

void CPU::LDHmnA() { write8(0xFF00 + fetch(), m_a); }

void CPU::POPHL() {
    uint16_t hl = pop16();
    m_h = (hl & 0xFF00) >> 8;
    m_l = hl & 0x00FF;
}

void CPU::LDmCA() { write8(0xFF00 + m_c, m_a); }
void CPU::PUSHHL() { push16(PAIR(m_h, m_l)); }
void CPU::ANDn() { _and(fetch()); }

void CPU::RST20H() {
    push16(m_pc);
    m_pc = 0x0020;
}

void CPU::ADDSPn() {
    int8_t operand = (int8_t) fetch();
    int32_t sum = m_sp + operand;
    (((m_sp ^ operand ^ (sum & 0xFFFF)) & 0x0010) == 0x0010) ? setBit(HALF) : clrBit(HALF);
    (((m_sp ^ operand ^ (sum & 0xFFFF)) & 0x0100) == 0x0100) ? setBit(CARRY) : clrBit(CARRY);
    clrBit(SIGN | ZERO);
    m_sp = static_cast<uint16_t>(sum);
}

void CPU::JPHL() { m_pc = PAIR(m_h, m_l); }

void CPU::LDmnnA() {
    uint16_t operand = read16(m_pc);
    m_pc += 2;
    write8(operand, m_a);
}

void CPU::XORn() { _xor(fetch()); }

void CPU::RST28H() {
    push16(m_pc);
    m_pc = 0x0028;
}

void CPU::LDHAmn() { m_a = read8(0xFF00 + fetch()); }

void CPU::POPAF() {
    uint16_t af = pop16();
    m_a = (af & 0xFF00) >> 8;
    m_f = af & 0x00F0; // lower nibble of F cannot be modified
}

void CPU::LDAmC() { m_a = read8(0xFF00 + m_c); }
void CPU::DI() { m_intMaster = false; }
void CPU::PUSHAF() { push16(PAIR(m_a, m_f)); }
void CPU::ORn() { _or(fetch()); }

void CPU::RST30H() {
    push16(m_pc);
    m_pc = 0x0030;
}

void CPU::LDHLSPn() {
    auto operand = static_cast<int8_t>(fetch());
    int32_t sum = m_sp + operand;

    (((m_sp ^ operand ^ (sum & 0xFFFF)) & 0x0010) == 0x0010) ? setBit(HALF) : clrBit(HALF);
    (((m_sp ^ operand ^ (sum & 0xFFFF)) & 0x0100) == 0x0100) ? setBit(CARRY) : clrBit(CARRY);
    clrBit(SIGN | ZERO);
    m_h = (sum & 0xFF00) >> 8;
    m_l = sum & 0x00FF;
}

void CPU::LDSPHL() { m_sp = PAIR(m_h, m_l); }

void CPU::LDAmnn() {
    uint16_t operand = read16(m_pc);
    m_pc += 2;
    m_a = read8(operand);
}

void CPU::EI() { m_intMaster = true; }
void CPU::CPn() { _cp(fetch()); }

void CPU::RST38H() {
    push16(m_pc);
    m_pc = 0x0038;
}

void CPU::RLCB() { m_b = _rlc(m_b); }
void CPU::RLCC() { m_c = _rlc(m_c); }
void CPU::RLCD() { m_d = _rlc(m_d); }
void CPU::RLCE() { m_e = _rlc(m_e); }
void CPU::RLCH() { m_h = _rlc(m_h); }
void CPU::RLCL() { m_l = _rlc(m_l); }
void CPU::RLCmHL() { write8(PAIR(m_h, m_l), _rlc(read8(PAIR(m_h, m_l)))); }
void CPU::RLC_A() { m_a = _rlc(m_a); }
void CPU::RRCB() { m_b = _rrc(m_b); }
void CPU::RRCC() { m_c = _rrc(m_c); }
void CPU::RRCD() { m_d = _rrc(m_d); }
void CPU::RRCE() { m_e = _rrc(m_e); }
void CPU::RRCH() { m_h = _rrc(m_h); }
void CPU::RRCL() { m_l = _rrc(m_l); }
void CPU::RRCmHL() { write8(PAIR(m_h, m_l), _rrc(read8(PAIR(m_h, m_l)))); }
void CPU::RRC_A() { m_a = _rrc(m_a); }
void CPU::RLB() { m_b = _rl(m_b); }
void CPU::RLC() { m_c = _rl(m_c); }
void CPU::RLD() { m_d = _rl(m_d); }
void CPU::RLE() { m_e = _rl(m_e); }
void CPU::RLH() { m_h = _rl(m_h); }
void CPU::RLL() { m_l = _rl(m_l); }
void CPU::RLmHL() { write8(PAIR(m_h, m_l), _rl(read8(PAIR(m_h, m_l)))); }
void CPU::RL_A() { m_a = _rl(m_a); }
void CPU::RRB() { m_b = _rr(m_b); }
void CPU::RRC() { m_c = _rr(m_c); }
void CPU::RRD() { m_d = _rr(m_d); }
void CPU::RRE() { m_e = _rr(m_e); }
void CPU::RRH() { m_h = _rr(m_h); }
void CPU::RRL() { m_l = _rr(m_l); }
void CPU::RRmHL() { write8(PAIR(m_h, m_l), _rr(read8(PAIR(m_h, m_l)))); }
void CPU::RR_A() { m_a = _rr(m_a); }
void CPU::SLAB() { m_b = _sla(m_b); }
void CPU::SLAC() { m_c = _sla(m_c); }
void CPU::SLAD() { m_d = _sla(m_d); }
void CPU::SLAE() { m_e = _sla(m_e); }
void CPU::SLAH() { m_h = _sla(m_h); }
void CPU::SLAL() { m_l = _sla(m_l); }
void CPU::SLAmHL() { write8(PAIR(m_h, m_l), _sla(read8(PAIR(m_h, m_l)))); }
void CPU::SLAA() { m_a = _sla(m_a); }
void CPU::SRAB() { m_b = _sra(m_b); }
void CPU::SRAC() { m_c = _sra(m_c); }
void CPU::SRAD() { m_d = _sra(m_d); }
void CPU::SRAE() { m_e = _sra(m_e); }
void CPU::SRAH() { m_h = _sra(m_h); }
void CPU::SRAL() { m_l = _sra(m_l); }
void CPU::SRAmHL() { write8(PAIR(m_h, m_l), _sra(read8(PAIR(m_h, m_l)))); }
void CPU::SRAA() { m_a = _sra(m_a); }
void CPU::SWAPB() { m_b = _swap(m_b); }
void CPU::SWAPC() { m_c = _swap(m_c); }
void CPU::SWAPD() { m_d = _swap(m_d); }
void CPU::SWAPE() { m_e = _swap(m_e); }
void CPU::SWAPH() { m_h = _swap(m_h); }
void CPU::SWAPL() { m_l = _swap(m_l); }
void CPU::SWAPmHL() { write8(PAIR(m_h, m_l), _swap(read8(PAIR(m_h, m_l)))); }
void CPU::SWAPA() { m_a = _swap(m_a); }
void CPU::SRLB() { m_b = _srl(m_b); }
void CPU::SRLC() { m_c = _srl(m_c); }
void CPU::SRLD() { m_d = _srl(m_d); }
void CPU::SRLE() { m_e = _srl(m_e); }
void CPU::SRLH() { m_h = _srl(m_h); }
void CPU::SRLL() { m_l = _srl(m_l); }
void CPU::SRLmHL() { write8(PAIR(m_h, m_l), _srl(read8(PAIR(m_h, m_l)))); }
void CPU::SRLA() { m_a = _srl(m_a); }
void CPU::BIT0B() { _bit(1, m_b); }
void CPU::BIT0C() { _bit(1, m_c); }
void CPU::BIT0D() { _bit(1, m_d); }
void CPU::BIT0E() { _bit(1, m_e); }
void CPU::BIT0H() { _bit(1, m_h); }
void CPU::BIT0L() { _bit(1, m_l); }
void CPU::BIT0mHL() { _bit(1, read8(PAIR(m_h, m_l))); }
void CPU::BIT0A() { _bit(1, m_a); }
void CPU::BIT1B() { _bit(2, m_b); }
void CPU::BIT1C() { _bit(2, m_c); }
void CPU::BIT1D() { _bit(2, m_d); }
void CPU::BIT1E() { _bit(2, m_e); }
void CPU::BIT1H() { _bit(2, m_h); }
void CPU::BIT1L() { _bit(2, m_l); }
void CPU::BIT1mHL() { _bit(2, read8(PAIR(m_h, m_l))); }
void CPU::BIT1A() { _bit(2, m_a); }
void CPU::BIT2B() { _bit(4, m_b); }
void CPU::BIT2C() { _bit(4, m_c); }
void CPU::BIT2D() { _bit(4, m_d); }
void CPU::BIT2E() { _bit(4, m_e); }
void CPU::BIT2H() { _bit(4, m_h); }
void CPU::BIT2L() { _bit(4, m_l); }
void CPU::BIT2mHL() { _bit(4, read8(PAIR(m_h, m_l))); }
void CPU::BIT2A() { _bit(4, m_a); }
void CPU::BIT3B() { _bit(8, m_b); }
void CPU::BIT3C() { _bit(8, m_c); }
void CPU::BIT3D() { _bit(8, m_d); }
void CPU::BIT3E() { _bit(8, m_e); }
void CPU::BIT3H() { _bit(8, m_h); }
void CPU::BIT3L() { _bit(8, m_l); }
void CPU::BIT3mHL() { _bit(8, read8(PAIR(m_h, m_l))); }
void CPU::BIT3A() { _bit(8, m_a); }
void CPU::BIT4B() { _bit(16, m_b); }
void CPU::BIT4C() { _bit(16, m_c); }
void CPU::BIT4D() { _bit(16, m_d); }
void CPU::BIT4E() { _bit(16, m_e); }
void CPU::BIT4H() { _bit(16, m_h); }
void CPU::BIT4L() { _bit(16, m_l); }
void CPU::BIT4mHL() { _bit(16, read8(PAIR(m_h, m_l))); }
void CPU::BIT4A() { _bit(16, m_a); }
void CPU::BIT5B() { _bit(32, m_b); }
void CPU::BIT5C() { _bit(32, m_c); }
void CPU::BIT5D() { _bit(32, m_d); }
void CPU::BIT5E() { _bit(32, m_e); }
void CPU::BIT5H() { _bit(32, m_h); }
void CPU::BIT5L() { _bit(32, m_l); }
void CPU::BIT5mHL() { _bit(32, read8(PAIR(m_h, m_l))); }
void CPU::BIT5A() { _bit(32, m_a); }
void CPU::BIT6B() { _bit(64, m_b); }
void CPU::BIT6C() { _bit(64, m_c); }
void CPU::BIT6D() { _bit(64, m_d); }
void CPU::BIT6E() { _bit(64, m_e); }
void CPU::BIT6H() { _bit(64, m_h); }
void CPU::BIT6L() { _bit(64, m_l); }
void CPU::BIT6mHL() { _bit(64, read8(PAIR(m_h, m_l))); }
void CPU::BIT6A() { _bit(64, m_a); }
void CPU::BIT7B() { _bit(128, m_b); }
void CPU::BIT7C() { _bit(128, m_c); }
void CPU::BIT7D() { _bit(128, m_d); }
void CPU::BIT7E() { _bit(128, m_e); }
void CPU::BIT7H() { _bit(128, m_h); }
void CPU::BIT7L() { _bit(128, m_l); }
void CPU::BIT7mHL() { _bit(128, read8(PAIR(m_h, m_l))); }
void CPU::BIT7A() { _bit(128, m_a); }
void CPU::RES0B() { m_b &= ~0x01; }
void CPU::RES0C() { m_c &= ~0x01; }
void CPU::RES0D() { m_d &= ~0x01; }
void CPU::RES0E() { m_e &= ~0x01; }
void CPU::RES0H() { m_h &= ~0x01; }
void CPU::RES0L() { m_l &= ~0x01; }
void CPU::RES0mHL() { write8(PAIR(m_h, m_l), read8(PAIR(m_h, m_l)) & ~0x01); }
void CPU::RES0A() { m_a &= ~0x01; }
void CPU::RES1B() { m_b &= ~0x02; }
void CPU::RES1C() { m_c &= ~0x02; }
void CPU::RES1D() { m_d &= ~0x02; }
void CPU::RES1E() { m_e &= ~0x02; }
void CPU::RES1H() { m_h &= ~0x02; }
void CPU::RES1L() { m_l &= ~0x02; }
void CPU::RES1mHL() { write8(PAIR(m_h, m_l), read8(PAIR(m_h, m_l)) & ~0x02); }
void CPU::RES1A() { m_a &= ~0x02; }
void CPU::RES2B() { m_b &= ~0x04; }
void CPU::RES2C() { m_c &= ~0x04; }
void CPU::RES2D() { m_d &= ~0x04; }
void CPU::RES2E() { m_e &= ~0x04; }
void CPU::RES2H() { m_h &= ~0x04; }
void CPU::RES2L() { m_l &= ~0x04; }
void CPU::RES2mHL() { write8(PAIR(m_h, m_l), read8(PAIR(m_h, m_l)) & ~0x04); }
void CPU::RES2A() { m_a &= ~0x04; }
void CPU::RES3B() { m_b &= ~0x08; }
void CPU::RES3C() { m_c &= ~0x08; }
void CPU::RES3D() { m_d &= ~0x08; }
void CPU::RES3E() { m_e &= ~0x08; }
void CPU::RES3H() { m_h &= ~0x08; }
void CPU::RES3L() { m_l &= ~0x08; }
void CPU::RES3mHL() { write8(PAIR(m_h, m_l), read8(PAIR(m_h, m_l)) & ~0x08); }
void CPU::RES3A() { m_a &= ~0x08; }
void CPU::RES4B() { m_b &= ~0x10; }
void CPU::RES4C() { m_c &= ~0x10; }
void CPU::RES4D() { m_d &= ~0x10; }
void CPU::RES4E() { m_e &= ~0x10; }
void CPU::RES4H() { m_h &= ~0x10; }
void CPU::RES4L() { m_l &= ~0x10; }
void CPU::RES4mHL() { write8(PAIR(m_h, m_l), read8(PAIR(m_h, m_l)) & ~0x10); }
void CPU::RES4A() { m_a &= ~0x10; }
void CPU::RES5B() { m_b &= ~0x20; }
void CPU::RES5C() { m_c &= ~0x20; }
void CPU::RES5D() { m_d &= ~0x20; }
void CPU::RES5E() { m_e &= ~0x20; }
void CPU::RES5H() { m_h &= ~0x20; }
void CPU::RES5L() { m_l &= ~0x20; }
void CPU::RES5mHL() { write8(PAIR(m_h, m_l), read8(PAIR(m_h, m_l)) & ~0x20); }
void CPU::RES5A() { m_a &= ~0x20; }
void CPU::RES6B() { m_b &= ~0x40; }
void CPU::RES6C() { m_c &= ~0x40; }
void CPU::RES6D() { m_d &= ~0x40; }
void CPU::RES6E() { m_e &= ~0x40; }
void CPU::RES6H() { m_h &= ~0x40; }
void CPU::RES6L() { m_l &= ~0x40; }
void CPU::RES6mHL() { write8(PAIR(m_h, m_l), read8(PAIR(m_h, m_l)) & ~0x40); }
void CPU::RES6A() { m_a &= ~0x40; }
void CPU::RES7B() { m_b &= ~0x80; }
void CPU::RES7C() { m_c &= ~0x80; }
void CPU::RES7D() { m_d &= ~0x80; }
void CPU::RES7E() { m_e &= ~0x80; }
void CPU::RES7H() { m_h &= ~0x80; }
void CPU::RES7L() { m_l &= ~0x80; }
void CPU::RES7mHL() { write8(PAIR(m_h, m_l), read8(PAIR(m_h, m_l)) & ~0x80); }
void CPU::RES7A() { m_a &= ~0x80; }
void CPU::SET0B() { m_b = _setBit(1, m_b); }
void CPU::SET0C() { m_c = _setBit(1, m_c); }
void CPU::SET0D() { m_d = _setBit(1, m_d); }
void CPU::SET0E() { m_e = _setBit(1, m_e); }
void CPU::SET0H() { m_h = _setBit(1, m_h); }
void CPU::SET0L() { m_l = _setBit(1, m_l); }
void CPU::SET0mHL() { write8(PAIR(m_h, m_l), _setBit(1, read8(PAIR(m_h, m_l)))); }
void CPU::SET0A() { m_a = _setBit(1, m_a); }
void CPU::SET1B() { m_b = _setBit(2, m_b); }
void CPU::SET1C() { m_c = _setBit(2, m_c); }
void CPU::SET1D() { m_d = _setBit(2, m_d); }
void CPU::SET1E() { m_e = _setBit(2, m_e); }
void CPU::SET1H() { m_h = _setBit(2, m_h); }
void CPU::SET1L() { m_l = _setBit(2, m_l); }
void CPU::SET1mHL() { write8(PAIR(m_h, m_l), _setBit(2, read8(PAIR(m_h, m_l)))); }
void CPU::SET1A() { m_a = _setBit(2, m_a); }
void CPU::SET2B() { m_b = _setBit(4, m_b); }
void CPU::SET2C() { m_c = _setBit(4, m_c); }
void CPU::SET2D() { m_d = _setBit(4, m_d); }
void CPU::SET2E() { m_e = _setBit(4, m_e); }
void CPU::SET2H() { m_h = _setBit(4, m_h); }
void CPU::SET2L() { m_l = _setBit(4, m_l); }
void CPU::SET2mHL() { write8(PAIR(m_h, m_l), _setBit(4, read8(PAIR(m_h, m_l)))); }
void CPU::SET2A() { m_a = _setBit(4, m_a); }
void CPU::SET3B() { m_b = _setBit(8, m_b); }
void CPU::SET3C() { m_c = _setBit(8, m_c); }
void CPU::SET3D() { m_d = _setBit(8, m_d); }
void CPU::SET3E() { m_e = _setBit(8, m_e); }
void CPU::SET3H() { m_h = _setBit(8, m_h); }
void CPU::SET3L() { m_l = _setBit(8, m_l); }
void CPU::SET3mHL() { write8(PAIR(m_h, m_l), _setBit(8, read8(PAIR(m_h, m_l)))); }
void CPU::SET3A() { m_a = _setBit(8, m_a); }
void CPU::SET4B() { m_b = _setBit(16, m_b); }
void CPU::SET4C() { m_c = _setBit(16, m_c); }
void CPU::SET4D() { m_d = _setBit(16, m_d); }
void CPU::SET4E() { m_e = _setBit(16, m_e); }
void CPU::SET4H() { m_h = _setBit(16, m_h); }
void CPU::SET4L() { m_l = _setBit(16, m_l); }
void CPU::SET4mHL() { write8(PAIR(m_h, m_l), _setBit(16, read8(PAIR(m_h, m_l)))); }
void CPU::SET4A() { m_a = _setBit(16, m_a); }
void CPU::SET5B() { m_b = _setBit(32, m_b); }
void CPU::SET5C() { m_c = _setBit(32, m_c); }
void CPU::SET5D() { m_d = _setBit(32, m_d); }
void CPU::SET5E() { m_e = _setBit(32, m_e); }
void CPU::SET5H() { m_h = _setBit(32, m_h); }
void CPU::SET5L() { m_l = _setBit(32, m_l); }
void CPU::SET5mHL() { write8(PAIR(m_h, m_l), _setBit(32, read8(PAIR(m_h, m_l)))); }
void CPU::SET5A() { m_a = _setBit(32, m_a); }
void CPU::SET6B() { m_b = _setBit(64, m_b); }
void CPU::SET6C() { m_c = _setBit(64, m_c); }
void CPU::SET6D() { m_d = _setBit(64, m_d); }
void CPU::SET6E() { m_e = _setBit(64, m_e); }
void CPU::SET6H() { m_h = _setBit(64, m_h); }
void CPU::SET6L() { m_l = _setBit(64, m_l); }
void CPU::SET6mHL() { write8(PAIR(m_h, m_l), _setBit(64, read8(PAIR(m_h, m_l)))); }
void CPU::SET6A() { m_a = _setBit(64, m_a); }
void CPU::SET7B() { m_b = _setBit(128, m_b); }
void CPU::SET7C() { m_c = _setBit(128, m_c); }
void CPU::SET7D() { m_d = _setBit(128, m_d); }
void CPU::SET7E() { m_e = _setBit(128, m_e); }
void CPU::SET7H() { m_h = _setBit(128, m_h); }
void CPU::SET7L() { m_l = _setBit(128, m_l); }
void CPU::SET7mHL() { write8(PAIR(m_h, m_l), _setBit(128, read8(PAIR(m_h, m_l)))); }
void CPU::SET7A() { m_a = _setBit(128, m_a); }

