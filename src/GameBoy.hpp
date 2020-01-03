#pragma once

#include <cstdint>
#include <memory>

#include "Cartridge.hpp"
#include "CPU.hpp"
#include "GPU.hpp"

#define INT_VBLANK 0x01
#define INT_LCDC   0x02
#define INT_TIMER  0x04
#define INT_SERIAL 0x08
#define INT_HITOLO 0x10

#define PC_VBLANK 0x0040
#define PC_LCDC   0x0048
#define PC_TIMER  0x0050
#define PC_SERIAL 0x0058
#define PC_HITOLO 0x0060

#define REG_IF 0xFF0F
#define REG_IE 0xFFFF

#define SIZE_CPU 0x10000

class GameBoy {
    public:
        GameBoy();
        ~GameBoy() = default;
        void reset();
        void step();

        uint8_t read8(uint16_t addr);
        uint16_t read16(uint16_t addr);
        void write8(uint16_t addr, uint8_t data);
        void write16(uint16_t addr, uint16_t data);

        CPU cpu;
        GPU gpu;
        std::array<uint8_t, SIZE_CPU> RAM;
        uint8_t intMaster = true;
        uint8_t intFlags = 0x00;
        uint8_t intEnable = 0x00;

        struct Keys {
            uint8_t a : 1;
            uint8_t b : 1;
            uint8_t select : 1;
            uint8_t start : 1;
            uint8_t right : 1;
            uint8_t left : 1;
            uint8_t up : 1;
            uint8_t down : 1;
        } keys;

    private:
        void DMA(uint16_t dst, uint16_t src, size_t size);
        void updateTile(uint16_t addr);
        void vblankISR();
        void lcdcISR();
        void timerISR();
        void serialISR();
        void hitoloISR();
};

