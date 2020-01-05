#pragma once

#include <cstdint>

#define DIV  0xFF04
#define TIMA 0xFF05
#define TMA  0xFF06
#define TAC  0xFF07
#define IF   0xFF0F

class GameBoy;

const int16_t freqCount[] = { 1024, 16, 64, 256 };

class Timer {
    public:
        Timer();
        ~Timer() = default;
        void connectGameBoy(GameBoy* g) { gb = g; }
        void reset();
        void step(uint32_t cycles);
        void setFrequency(uint8_t freq);

        bool running = true;
        uint8_t divider = 0x00;
        uint8_t timer = 0x00;
        uint8_t modulo = 0x00;
        uint32_t cycles = 0;
        uint8_t frequency = 0x00;

        GameBoy* gb;
};

