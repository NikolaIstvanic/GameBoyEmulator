#include <algorithm>

#include "GameBoy.hpp"

GameBoy::GameBoy() {
    cpu.connectGameBoy(this);
    gpu.connectGameBoy(this);
    std::fill(RAM.begin(), RAM.end(), 0x00);
}

void GameBoy::reset() {
    cpu.reset();
    gpu.reset();

    write8(0xFF07, 0xF8);
    write8(0xFF10, 0x80);
    write8(0xFF11, 0xBF);
    write8(0xFF12, 0xF3);
    write8(0xFF14, 0x8F);
    write8(0xFF16, 0x3F);
    write8(0xFF19, 0xBF);
    write8(0xFF1A, 0x7F);
    write8(0xFF1B, 0xFF);
    write8(0xFF1C, 0x9F);
    write8(0xFF1E, 0xBF);
    write8(0xFF20, 0xFF);
    write8(0xFF23, 0xBF);
    write8(0xFF24, 0x77);
    write8(0xFF25, 0xF3);
    write8(0xFF26, 0xF1);
    write8(0xFF40, 0x91);
    write8(0xFF41, 0x84);
    write8(0xFF47, 0xFC);
    write8(0xFF48, 0xFF);
    write8(0xFF49, 0xFF);

    keys.a = 1;
    keys.b = 1;
    keys.select = 1;
    keys.start = 1;
    keys.right = 1;
    keys.left = 1;
    keys.up = 1;
    keys.down = 1;
}

void GameBoy::step() {
    uint8_t cycles = cpu.step();
    gpu.step(cycles);
    cpu.timer.step(cycles);
}

uint8_t GameBoy::read8(uint16_t addr) {
    if (addr == 0xFF00) {
        if (!(RAM[0xFF00] & 0x20)) {
            // Buttons
            return 0xD0 | ((keys.start << 3) | (keys.select << 2) | (keys.b << 1) | keys.a);
        } else if (!(RAM[0xFF00] & 0x10)) {
            // Directional keys
            return 0xE0 | ((keys.down << 3) | (keys.up << 2) | (keys.left << 1) | keys.right);
        } else if (!(RAM[0xFF00] & 0x30)) {
            return 0xFF;
        }
    } else if (addr == 0xFF04) {
        return cpu.timer.divCount;
    } else if (addr == 0xFF05) {
        return cpu.timer.timerCount;
    } else if (addr == 0xFF06) {
        return cpu.timer.modulo;
    } else if (addr == 0xFF07) {
        return cpu.timer.control;
    } else if (addr == 0xFF0F) {
        return cpu.intFlags;
    } else if (addr == 0xFF40) {
        return gpu.lcdControl;
    } else if (addr == 0xFF41) {
        return gpu.lcdStatus;
    } else if (addr == 0xFF42) {
        return gpu.scrollY;
    } else if (addr == 0xFF43) {
        return gpu.scrollX;
    } else if (addr == 0xFF44) {
        return gpu.scanline;
    } else if (addr == 0xFFFF) {
        return cpu.intEnable;
    }
    return RAM[addr];
}

uint16_t GameBoy::read16(uint16_t addr) { return (read8(addr + 1) << 8) | read8(addr); }

void GameBoy::write8(uint16_t addr, uint8_t data) {
    RAM[addr] = data;
    if (0x8000 <= addr && addr <= 0x97FF) {
        updateTile(addr);
    } else if (addr == 0xFF04) {
        cpu.timer.divCount = 0x00;
    } else if (addr == 0xFF05) {
        cpu.timer.timerCount = data;
    } else if (addr == 0xFF06) {
        cpu.timer.modulo = data;
    } else if (addr == 0xFF07) {
        cpu.timer.control = data;
    } else if (addr == 0xFF0F) {
        cpu.intFlags = data;
    } else if (addr == 0xFF40) {
        gpu.lcdControl = data;
    } else if (addr == 0xFF42) {
        gpu.scrollY = data;
    } else if (addr == 0xFF43) {
        gpu.scrollX = data;
    } else if (addr == 0xFF44) {
        RAM[addr] = 0x00;
    } else if (addr == 0xFF46) {
        DMA(0xFE00, data << 8, 160);
    } else if (addr == 0xFF47) {
        for (int i = 0; i < 4; i++) {
            gpu.bg[i] = gpu.colorPalette[(data >> (i * 2)) & 0x03];
        }
    } else if (addr == 0xFF48) {
        for (int i = 0; i < 4; i++) {
            gpu.spritePalette[0][i] = gpu.colorPalette[(data >> (i * 2)) & 0x03];
        }
    } else if (addr == 0xFF49) {
        for (int i = 0; i < 4; i++) {
            gpu.spritePalette[1][i] = gpu.colorPalette[(data >> (i * 2)) & 0x03];
        }
    } else if (addr == 0xFFFF) {
        cpu.intEnable = data;
    }
}

void GameBoy::write16(uint16_t addr, uint16_t data) {
    write8(addr, data & 0x00FF);
    write8(addr + 1, data >> 8);
}

void GameBoy::DMA(uint16_t dst, uint16_t src, size_t size) {
    for (uint32_t i = 0; i < size; i++) {
        write8(dst + i, read8(src + i));
    }
}

void GameBoy::updateTile(uint16_t addr) {
    addr &= 0x1FFE;
    uint8_t mask;
    uint16_t tile = (addr >> 4) & 0x01FF;
    uint16_t y = (addr >> 1) & 0x07;

    for (int i = 0; i < 8; i++) {
        mask = 1 << (7 - i);
        gpu.tiles[tile][y][i] = ((RAM[0x8000 + addr] & mask) ? 1 : 0)
            + ((RAM[0x8000 + addr + 1] & mask) ? 2 : 0);
    }
}

