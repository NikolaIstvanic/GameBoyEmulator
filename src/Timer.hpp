#pragma once

#include <cstdint>

class GameBoy;

class Timer {
public:
    Timer();
    ~Timer();

    void connectGameBoy(GameBoy* gb);
    void reset();
    void step(uint32_t cycles);

    bool m_running = true;
    int m_timerCount = 0;
    uint8_t m_modulo = 0x00;
    uint8_t m_control = 0x00;
    uint8_t m_timerValue = 0x00;

    int m_divCount = 0x00;

    GameBoy* m_gb = nullptr;
};

