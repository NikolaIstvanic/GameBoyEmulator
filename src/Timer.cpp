#include "GameBoy.hpp"
#include "Timer.hpp"

#define DIV  0xFF04
#define TIMA 0xFF05
#define TMA  0xFF06
#define TAC  0xFF07
#define IF   0xFF0F

#define FREQ4096  0x00
#define FREQ16384 0x03

static const int16_t g_freqCount[] = { 1024, 16, 64, 256 };

Timer::Timer() {
    reset();
}

Timer::~Timer() = default;

void Timer::connectGameBoy(GameBoy* gb) {
    m_gb = gb;
}

void Timer::reset() {
    m_running = true;
    m_timerCount = static_cast<uint8_t>(g_freqCount[FREQ4096]);
    m_modulo = 0x00;
    m_control = 0x00;
    m_timerValue = 0x00;

    m_divCount = static_cast<uint8_t>(g_freqCount[FREQ16384]);
}

void Timer::step(uint32_t cycles) {
    m_divCount -= cycles;
    while (m_divCount <= 0) {
        m_divCount += g_freqCount[FREQ16384];
    }

    if (m_control & 0x04) {
        m_timerCount -= cycles;
        while (m_timerCount <= 0) {
            m_timerCount += g_freqCount[m_control & 0x03];
            m_timerValue++;
            if (!m_timerValue) {
                m_timerValue = m_modulo;
                m_gb->m_cpu.m_intFlags |= INT_TIMER;
            }
        }
    }
}

