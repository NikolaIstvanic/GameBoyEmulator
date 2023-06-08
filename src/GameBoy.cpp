#include <algorithm>

#include "GameBoy.hpp"

#define REG_IF 0xFF0F
#define REG_IE 0xFFFF

GameBoy::GameBoy() {
    m_cpu.connectGameBoy(this);
    m_gpu.connectGameBoy(this);
    std::fill(m_ram.begin(), m_ram.end(), 0x00);
}

GameBoy::~GameBoy() = default;

void GameBoy::reset() {
    m_cpu.reset();
    m_gpu.reset();

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

    m_keys.a = 1;
    m_keys.b = 1;
    m_keys.select = 1;
    m_keys.start = 1;
    m_keys.right = 1;
    m_keys.left = 1;
    m_keys.up = 1;
    m_keys.down = 1;
}

void GameBoy::step() {
    uint8_t cycles = m_cpu.step();
    m_gpu.step(cycles);
    m_cpu.m_timer.step(cycles);
}

uint8_t GameBoy::read8(uint16_t addr) {
    if (addr == 0xFF00) {
        if (!(m_ram[0xFF00] & 0x20)) {
            // Buttons
            return 0xD0 | ((m_keys.start << 3) | (m_keys.select << 2) | (m_keys.b << 1) | m_keys.a);
        } else if (!(m_ram[0xFF00] & 0x10)) {
            // Directional keys
            return 0xE0 | ((m_keys.down << 3) | (m_keys.up << 2) | (m_keys.left << 1) | m_keys.right);
        } else if (!(m_ram[0xFF00] & 0x30)) {
            return 0xFF;
        }
    } else if (addr == 0xFF04) {
        return m_cpu.m_timer.m_divCount;
    } else if (addr == 0xFF05) {
        return m_cpu.m_timer.m_timerCount;
    } else if (addr == 0xFF06) {
        return m_cpu.m_timer.m_modulo;
    } else if (addr == 0xFF07) {
        return m_cpu.m_timer.m_control;
    } else if (addr == 0xFF0F) {
        return m_cpu.m_intFlags;
    } else if (addr == 0xFF40) {
        return m_gpu.m_lcdControl;
    } else if (addr == 0xFF41) {
        return m_gpu.m_lcdStatus;
    } else if (addr == 0xFF42) {
        return m_gpu.m_scrollY;
    } else if (addr == 0xFF43) {
        return m_gpu.m_scrollX;
    } else if (addr == 0xFF44) {
        return m_gpu.m_scanline;
    } else if (addr == 0xFFFF) {
        return m_cpu.m_intEnable;
    }

    return m_ram[addr];
}

uint16_t GameBoy::read16(uint16_t addr) {
    return (read8(addr + 1) << 8) | read8(addr);
}

void GameBoy::write8(uint16_t addr, uint8_t data) {
    m_ram[addr] = data;
    if (0x8000 <= addr && addr <= 0x97FF) {
        updateTile(addr);
    } else if (addr == 0xFF04) {
        m_cpu.m_timer.m_divCount = 0x00;
    } else if (addr == 0xFF05) {
        m_cpu.m_timer.m_timerCount = data;
    } else if (addr == 0xFF06) {
        m_cpu.m_timer.m_modulo = data;
    } else if (addr == 0xFF07) {
        m_cpu.m_timer.m_control = data;
    } else if (addr == 0xFF0F) {
        m_cpu.m_intFlags = data;
    } else if (addr == 0xFF40) {
        m_gpu.m_lcdControl = data;
    } else if (addr == 0xFF42) {
        m_gpu.m_scrollY = data;
    } else if (addr == 0xFF43) {
        m_gpu.m_scrollX = data;
    } else if (addr == 0xFF44) {
        m_ram[addr] = 0x00;
    } else if (addr == 0xFF46) {
        DMA(0xFE00, data << 8, 160);
    } else if (addr == 0xFF47) {
        for (int i = 0; i < 4; i++) {
            m_gpu.m_bg[i] = m_gpu.m_colorPalette[(data >> (i * 2)) & 0x03];
        }
    } else if (addr == 0xFF48) {
        for (int i = 0; i < 4; i++) {
            m_gpu.m_spritePalette[0][i] = m_gpu.m_colorPalette[(data >> (i * 2)) & 0x03];
        }
    } else if (addr == 0xFF49) {
        for (int i = 0; i < 4; i++) {
            m_gpu.m_spritePalette[1][i] = m_gpu.m_colorPalette[(data >> (i * 2)) & 0x03];
        }
    } else if (addr == 0xFFFF) {
        m_cpu.m_intEnable = data;
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
        m_gpu.m_tiles[tile][y][i] = ((m_ram[0x8000 + addr] & mask) ? 1 : 0)
            + ((m_ram[0x8000 + addr + 1] & mask) ? 2 : 0);
    }
}

