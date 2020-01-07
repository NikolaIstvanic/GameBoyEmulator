#include "GameBoy.hpp"
#include "Timer.hpp"

Timer::Timer() { reset(); }

void Timer::reset() {
    this->running = true;
    this->timerCount = (uint8_t) freqCount[FREQ4096];
    this->modulo = 0x00;
    this->control = 0x00;
    this->timerValue = 0x00;

    this->divCount = (uint8_t) freqCount[FREQ16384];
}

void Timer::step(uint32_t cycles) {
    this->divCount -= cycles;
    while (this->divCount <= 0) {
        this->divCount += freqCount[FREQ16384];
    }

    if (this->control & 0x04) {
        this->timerCount -= cycles;
        while (this->timerCount <= 0) {
            this->timerCount += freqCount[this->control & 0x03];
            this->timerValue++;
            if (!this->timerValue) {
                this->timerValue = this->modulo;
                this->gb->cpu.intFlags |= INT_TIMER;
            }
        }
    }
}

