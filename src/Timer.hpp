#pragma once

#include <cstdint>

#define DIV  0xFF04
#define TIMA 0xFF05
#define TMA  0xFF06
#define TAC  0xFF07
#define IF   0xFF0F

class Timer {
    public:
        Timer();
        ~Timer() = default;




        const uint16_t TimeDividers[4] = { 1024, 16, 64, 256 };

};

