#pragma once

#include <cstdint>
#include <memory>

#include "CPU.hpp"
#include "GPU.hpp"

#define SIZE_CPU 0x10000

class GameBoy {
public:
    GameBoy();
    ~GameBoy();

    void reset();
    void step();

    uint8_t read8(uint16_t addr);
    uint16_t read16(uint16_t addr);
    void write8(uint16_t addr, uint8_t data);
    void write16(uint16_t addr, uint16_t data);

    CPU m_cpu;
    GPU m_gpu;
    std::array<uint8_t, SIZE_CPU> m_ram;

    struct Keys {
        uint8_t a : 1;
        uint8_t b : 1;
        uint8_t select : 1;
        uint8_t start : 1;
        uint8_t right : 1;
        uint8_t left : 1;
        uint8_t up : 1;
        uint8_t down : 1;
    } m_keys;

private:
    void DMA(uint16_t dst, uint16_t src, size_t size);
    void updateTile(uint16_t addr);
};

