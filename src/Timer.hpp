#pragma once

#include <cstdint>

#define DIV  0xFF04
#define TIMA 0xFF05
#define TMA  0xFF06
#define TAC  0xFF07
#define IF   0xFF0F

#define FREQ4096  0x00
#define FREQ16384 0x03

class GameBoy;

const int16_t freqCount[] = { 1024, 16, 64, 256 };

class Timer {
    public:
        Timer();
        ~Timer() = default;
        void connectGameBoy(GameBoy* g) { gb = g; }
        void reset();
        void step(uint32_t cycles);

        bool running = true;
        int timerCount = 0;
        uint8_t modulo = 0x00;
        uint8_t control = 0x00;
        uint8_t timerValue = 0x00;

        int divCount = 0x00;

        GameBoy* gb;
};

