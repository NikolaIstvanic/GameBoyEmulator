#include "GameBoy.hpp"
#include "Timer.hpp"

Timer::Timer() { reset(); }

void Timer::reset() {
    this->running = true;
    this->frequency = 0x00;
    this->cycles = freqCount[this->frequency];
}

void Timer::step(uint32_t cycles) {
    if (running) {
        divider += cycles;
        this->cycles -= cycles;
        uint8_t value = gb->read8(TIMA) + 1;
        if (value == 0x00) {
            gb->cpu.intFlags |= INT_TIMER;
        }
        gb->write8(TIMA, value);
    }
}

void Timer::setFrequency(uint8_t freq) {
    this->frequency = freq;
    this->cycles = freqCount[this->frequency];
}

