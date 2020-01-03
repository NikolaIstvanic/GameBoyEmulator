#pragma once

#include <array>
#include <cstdint>

//#include "Timer.hpp"

class GameBoy;

class CPU {
    public:
        CPU();
        ~CPU() = default;
        void connectGameBoy(GameBoy* g) { gb = g; /*timer.connectGameBoy(g);*/ }
        void reset();
        void step();

        void push8(uint8_t data);
        void push16(uint16_t data);
        
        uint8_t cycles = 0;

        // STATUS REGISTER VALUES
        enum CPUFLAG {
            ZERO = 0x80,
            SIGN = 0x40,
            HALF = 0x20,
            CARRY = 0x10
        };

        // Whether stopped
        bool stop = false;

        // Whether halting
        bool halt = false;

        //Interrupt logic
        uint8_t intMaster = true;

        // Program Counter address register
        uint16_t PC = 0x0000;

    private:
        inline void logInfo() const;
        inline uint8_t fetch();
        inline uint16_t relativeOffset(uint8_t offset) const;
        inline void setBit(uint8_t f);
        inline void clrBit(uint8_t f);
        inline void setZEROSIGN(uint8_t value);

        void write8(uint16_t addr, uint8_t data);
        void write16(uint16_t addr, uint16_t data);
        uint8_t read8(uint16_t addr);
        uint16_t read16(uint16_t addr);
        uint8_t pop8();
        uint16_t pop16();

        uint8_t _add8(uint8_t operand);
        uint16_t _add16(uint16_t operand);
        void _adc(uint8_t operand);
        void _sub(uint8_t operand);
        void _sbc(uint8_t operand);
        void _and(uint8_t operand);
        void _xor(uint8_t operand);
        void _or(uint8_t operand);
        void _cp(uint8_t operand);
        void _inc(uint8_t& operand);
        void _dec(uint8_t& operand);
        void _rlc(uint8_t& operand);
        void _rrc(uint8_t& operand);
        void _rl(uint8_t& operand);
        void _rr(uint8_t& operand);
        void _sla(uint8_t& operand);
        void _sra(uint8_t& operand);
        void _swap(uint8_t& operand);
        void _srl(uint8_t& operand);
        void _bit(uint8_t mask, uint8_t operand);
        void _setBit(uint8_t mask, uint8_t& operand) { operand |= mask; }

        // Instruction set methods (excluding illegal opcodes)
        void NOP(); void LDBCnn(); void LDmBCA(); void INCBC(); void INCB(); void DECB();
        void LDBn(); void RLCA(); void LDmnnSP(); void ADDHLBC(); void LDAmBC(); void DECBC();
        void INCC(); void DECC(); void LDCn(); void RRCA();
        void STOP(); void LDDEnn(); void LDmDEA(); void INCDE(); void INCD(); void DECD();
        void LDDn(); void RLA(); void JRn(); void ADDHLDE(); void LDAmDE(); void DECDE();
        void INCE(); void DECE(); void LDEn(); void RRA();
        void JRnzn(); void LDHLnn(); void LDmHLpA(); void INCHL(); void INCH(); void DECH();
        void LDHn(); void DAA(); void JRzn(); void ADDHLHL(); void LDAmHLp(); void DECHL();
        void INCL(); void DECL(); void LDLn(); void CPL();
        void JRncn(); void LDSPnn(); void LDmHLmA(); void INCSP(); void INCmHL(); void DECmHL();
        void LDmHLn(); void SCF(); void JRcn(); void ADDHLSP(); void LDAmHLm(); void DECSP();
        void INCA(); void DECA(); void LDAn(); void CCF();
        void LDBB(); void LDBC(); void LDBD(); void LDBE(); void LDBH(); void LDBL(); void LDBmHL();
        void LDBA(); void LDCB(); void LDCC(); void LDCD(); void LDCE(); void LDCH(); void LDCL();
        void LDCmHL(); void LDCA();
        void LDDB(); void LDDC(); void LDDD(); void LDDE(); void LDDH(); void LDDL();
        void LDDmHL(); void LDDA(); void LDEB(); void LDEC(); void LDED(); void LDEE();
        void LDEH(); void LDEL(); void LDEmHL(); void LDEA();
        void LDHB(); void LDHC(); void LDHD(); void LDHE(); void LDHH(); void LDHL();
        void LDHmHL(); void LDHA(); void LDLB(); void LDLC(); void LDLD(); void LDLE();
        void LDLH(); void LDLL(); void LDLmHL(); void LDLA();
        void LDmHLB(); void LDmHLC(); void LDmHLD(); void LDmHLE(); void LDmHLH(); void LDmHLL();
        void HALT(); void LDmHLA(); void LDAB(); void LDAC(); void LDAD(); void LDAE();
        void LDAH(); void LDAL(); void LDAmHL(); void LDAA();
        void ADDAB(); void ADDAC(); void ADDAD(); void ADDAE(); void ADDAH(); void ADDAL();
        void ADDAmHL(); void ADDAA(); void ADCAB(); void ADCAC(); void ADCAD();
        void ADCAE(); void ADCAH(); void ADCAL(); void ADCAmHL(); void ADCAA();
        void SUBB(); void SUBC(); void SUBD(); void SUBE(); void SUBH(); void SUBL(); void SUBmHL();
        void SUBA(); void SBCAB(); void SBCAC(); void SBCAD(); void SBCAE(); void SBCAH();
        void SBCAL(); void SBCAmHL(); void SBCAA();
        void ANDB(); void ANDC(); void ANDD(); void ANDE(); void ANDH(); void ANDL(); void ANDmHL();
        void ANDA(); void XORB(); void XORC(); void XORD(); void XORE(); void XORH(); void XORL();
        void XORmHL(); void XORA();
        void ORB(); void ORC(); void ORD(); void ORE(); void ORH(); void ORL(); void ORmHL();
        void ORA(); void CPB(); void CPC(); void CPD(); void CPE(); void CPH(); void CP_L();
        void CPmHL(); void CPA(); 
        void RETnz(); void POPBC(); void JPnznn(); void JPnn(); void CALLnznn(); void PUSHBC(); void ADDAn();
        void RST00H(); void RETz(); void RET(); void JPznn(); void CBn(); void CALLznn(); void CALLnn();
        void ADCAn(); void RST08H();
        void RETnc(); void POPDE(); void JPncnn(); void PANIC(); void CALLncnn(); void PUSHDE(); void SUBAn();
        void RST10H(); void RETc(); void RETI(); void JPcnn(); void CALLcnn(); void SBCAn(); void RST18H();
        void LDHmnA(); void POPHL(); void LDmCA(); void PUSHHL(); void ANDn(); void RST20H(); void ADDSPn();
        void JPHL(); void LDmnnA(); void XORn(); void RST28H();
        void LDHAmn(); void POPAF(); void LDAmC(); void DI(); void PUSHAF(); void ORn(); void RST30H();
        void LDHLSPn(); void LDSPHL(); void LDAmnn(); void EI(); void CPn(); void RST38H();

		void RLCB(); void RLCC(); void RLCD(); void RLCE(); void RLCH(); void RLCL(); void RLCmHL();
		void RLC_A(); void RRCB(); void RRCC();  void RRCD(); void RRCE(); void RRCH(); void RRCL();
		void RRCmHL(); void RRC_A(); void RLB(); void RLC(); void RLD(); void RLE(); void RLH(); void RLL();
        void RLmHL(); void RL_A(); void RRB(); void RRC(); void RRD(); void RRE(); void RRH(); void RRL();
        void RRmHL(); void RR_A(); void SLAB(); void SLAC(); void SLAD(); void SLAE(); void SLAH();
        void SLAL(); void SLAmHL(); void SLAA(); void SRAB(); void SRAC(); void SRAD(); void SRAE();
        void SRAH(); void SRAL(); void SRAmHL(); void SRAA(); void SWAPB(); void SWAPC(); void SWAPD();
        void SWAPE(); void SWAPH(); void SWAPL(); void SWAPmHL(); void SWAPA(); void SRLB(); void SRLC();
        void SRLD(); void SRLE(); void SRLH(); void SRLL(); void SRLmHL(); void SRLA();
        void BIT0B(); void BIT0C(); void BIT0D(); void BIT0E(); void BIT0H(); void BIT0L(); void BIT0mHL(); void BIT0A();
        void BIT1B(); void BIT1C(); void BIT1D(); void BIT1E(); void BIT1H(); void BIT1L(); void BIT1mHL(); void BIT1A();
        void BIT2B(); void BIT2C(); void BIT2D(); void BIT2E(); void BIT2H(); void BIT2L(); void BIT2mHL(); void BIT2A();
        void BIT3B(); void BIT3C(); void BIT3D(); void BIT3E(); void BIT3H(); void BIT3L(); void BIT3mHL(); void BIT3A();
        void BIT4B(); void BIT4C(); void BIT4D(); void BIT4E(); void BIT4H(); void BIT4L(); void BIT4mHL(); void BIT4A();
        void BIT5B(); void BIT5C(); void BIT5D(); void BIT5E(); void BIT5H(); void BIT5L(); void BIT5mHL(); void BIT5A();
        void BIT6B(); void BIT6C(); void BIT6D(); void BIT6E(); void BIT6H(); void BIT6L(); void BIT6mHL(); void BIT6A();
        void BIT7B(); void BIT7C(); void BIT7D(); void BIT7E(); void BIT7H(); void BIT7L(); void BIT7mHL(); void BIT7A();
        void RES0B(); void RES0C(); void RES0D(); void RES0E(); void RES0H(); void RES0L(); void RES0mHL(); void RES0A();
        void RES1B(); void RES1C(); void RES1D(); void RES1E(); void RES1H(); void RES1L(); void RES1mHL(); void RES1A();
        void RES2B(); void RES2C(); void RES2D(); void RES2E(); void RES2H(); void RES2L(); void RES2mHL(); void RES2A();
        void RES3B(); void RES3C(); void RES3D(); void RES3E(); void RES3H(); void RES3L(); void RES3mHL(); void RES3A();
        void RES4B(); void RES4C(); void RES4D(); void RES4E(); void RES4H(); void RES4L(); void RES4mHL(); void RES4A();
        void RES5B(); void RES5C(); void RES5D(); void RES5E(); void RES5H(); void RES5L(); void RES5mHL(); void RES5A();
        void RES6B(); void RES6C(); void RES6D(); void RES6E(); void RES6H(); void RES6L(); void RES6mHL(); void RES6A();
        void RES7B(); void RES7C(); void RES7D(); void RES7E(); void RES7H(); void RES7L(); void RES7mHL(); void RES7A();
        void SET0B(); void SET0C(); void SET0D(); void SET0E(); void SET0H(); void SET0L(); void SET0mHL(); void SET0A();
        void SET1B(); void SET1C(); void SET1D(); void SET1E(); void SET1H(); void SET1L(); void SET1mHL(); void SET1A();
        void SET2B(); void SET2C(); void SET2D(); void SET2E(); void SET2H(); void SET2L(); void SET2mHL(); void SET2A();
        void SET3B(); void SET3C(); void SET3D(); void SET3E(); void SET3H(); void SET3L(); void SET3mHL(); void SET3A();
        void SET4B(); void SET4C(); void SET4D(); void SET4E(); void SET4H(); void SET4L(); void SET4mHL(); void SET4A();
        void SET5B(); void SET5C(); void SET5D(); void SET5E(); void SET5H(); void SET5L(); void SET5mHL(); void SET5A();
        void SET6B(); void SET6C(); void SET6D(); void SET6E(); void SET6H(); void SET6L(); void SET6mHL(); void SET6A();
        void SET7B(); void SET7C(); void SET7D(); void SET7E(); void SET7H(); void SET7L(); void SET7mHL(); void SET7A();

        //Timer timer;
        GameBoy* gb;
        uint8_t additional_cycle = 0;

        // TODO; is this endianness ?
        union Register {
            struct {
                uint8_t r2;
                uint8_t r1;
            };
            uint16_t r = 0x0000;
        };

        Register AF;
        Register BC;
        Register DE;
        Register HL;

        // 16-bit stack pointer
        uint16_t SP = 0x00;

        // Current opcode
        uint8_t opcode = 0x00;

        struct instruction {
            std::string name;
            void (CPU::*op)(void);
            uint8_t cycles;
        };

        std::array<struct instruction, 0x100> instruction_rom;
        std::array<struct instruction, 0x100> cb_rom;
};

