#pragma once

#include <cstdint>
#include <memory>

#include "Cartridge.hpp"
#include "CPU.hpp"
#include "GPU.hpp"

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
};

