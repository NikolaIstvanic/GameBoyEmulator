#include <cstring>
#include <iostream>

#include "GameBoy.hpp"
#include "GPU.hpp"

#define SPRENABLE 0x02
#define TILEMAP 0x08

GPU::GPU() {
    m_colorPalette = {{
        olc::Pixel(155, 188, 15), olc::Pixel(139, 172, 15),
        olc::Pixel(48, 98, 48), olc::Pixel(15, 56, 15)
    }};
}

GPU::~GPU() = default;

void GPU::reset() {
    m_scanline = 0;
    m_scrollX = 0;
    m_scrollY = 0;
    m_clocks = 0;
    m_lcdControl = 0x00;
    m_lcdStatus = 0x00;
    m_lyCompare = 0x00;
    m_state = HBLANK;

    m_bg[0] = m_colorPalette[0];
    m_bg[1] = m_colorPalette[1];
    m_bg[2] = m_colorPalette[2];
    m_bg[3] = m_colorPalette[3];
    m_spritePalette[0][0] = m_colorPalette[0];
    m_spritePalette[0][1] = m_colorPalette[1];
    m_spritePalette[0][2] = m_colorPalette[2];
    m_spritePalette[0][3] = m_colorPalette[3];
    m_spritePalette[1][0] = m_colorPalette[0];
    m_spritePalette[1][1] = m_colorPalette[1];
    m_spritePalette[1][2] = m_colorPalette[2];
    m_spritePalette[1][3] = m_colorPalette[3];
    memset(m_tiles, 0x00, sizeof(m_tiles));
}

void GPU::connectGameBoy(GameBoy* gb) {
    m_gb = gb;
}

olc::Sprite& GPU::getScreen() {
    return m_sprScreen;
}

void GPU::step(uint8_t cpuCycles) {
    m_clocks += cpuCycles;

    switch (m_state) {
        case HBLANK:
            if (m_clocks >= 204) {
                m_scanline++;

                if (m_scanline == 144) {
                    // request vblank interrupt
                    m_gb->m_cpu.m_intFlags |= INT_VBLANK;
                    m_state = VBLANK;
                    m_lcdStatus &= ~0x1;
                    m_lcdStatus |= 0x2;
                    m_frameDone = true;
                } else {
                    m_state = OAM;
                    m_lcdStatus |= 0x2;
                    m_lcdStatus &= ~0x1;
                }

                m_clocks -= 204;
            }
            break;

        case VBLANK:
            if (m_clocks >= 456) {
                m_scanline++;

                if (m_scanline > 153) {
                    m_scanline = 0;
                    m_lcdStatus |= 0x2;
                    m_lcdStatus &= ~0x1;
                    m_state = OAM;
                }

                m_clocks -= 456;
            }
            break;

        case OAM:
            if (m_clocks >= 80) {
                m_clocks -= 80;
                m_lcdStatus |= 0x2;
                m_lcdStatus &= ~0x1;
                m_state = VRAM;
            }
            break;

        case VRAM:
            if (m_clocks >= 172) {
                m_state = HBLANK;
                updateScreen();
                m_clocks -= 172;

                if (m_lcdStatus & 0x08) {
                    m_gb->m_cpu.m_intFlags |= INT_LCDC;
                }

                uint8_t lyCoincidenceInt = m_lcdStatus & 0x40;
                uint8_t lyCoincidence = m_lyCompare == m_scanline;

                if (lyCoincidenceInt && lyCoincidence) {
                    m_gb->m_cpu.m_intFlags |= INT_LCDC;
                }

                lyCoincidence ? m_lcdStatus |= 0x4 : m_lcdStatus &= ~0x4;
                m_lcdStatus &= ~0x2;
                m_lcdStatus &= ~0x1;
            }
            break;
    }
}

void GPU::updateScreen() {
    uint16_t mapOffset = ((m_lcdControl & TILEMAP) ? 0x1C00 : 0x1800)
        + ((((m_scanline + m_scrollY) & 0xFF) >> 3) << 5);
    uint8_t lineOffset = m_scrollX >> 3;
    uint8_t x = m_scrollX & 0x07;
    uint8_t y = (m_scanline + m_scrollY) & 0x07;
    uint16_t tile = (uint16_t) (m_gb->read8(0x8000 + mapOffset + lineOffset));
    uint16_t scanlineRow[WIDTH];

    for (uint16_t i = 0; i < WIDTH; i++) {
        uint8_t color = m_tiles[tile][y][x];
        scanlineRow[i] = color;
        m_sprScreen.SetPixel(i, m_scanline, m_bg[color]);
        if (++x == 8) {
            x = 0;
            lineOffset = (lineOffset + 1) & 0x1F;
            tile = (int16_t) (m_gb->read8(0x8000 + mapOffset + lineOffset));
        }
    }

    for (uint8_t i = 0; i < 40; i++) {
        struct gpuSprite sprite = ((struct gpuSprite*) (m_gb->m_ram.data() + 0xFE00))[i];
        uint16_t sx = sprite.x - 8;
        uint16_t sy = sprite.y - 16;
        uint16_t offset = 0;

        if (sy <= m_scanline && (sy + 8) > m_scanline) {
            std::array<olc::Pixel, 4> p = m_spritePalette[sprite.palette];
            uint8_t tileRow;

            if (sprite.vFlip) {
                tileRow = 7 - (m_scanline - sy);
            } else {
                tileRow = m_scanline - sy;
            }

            for (int x = 0; x < 8; x++) {
                if (sx + x >= 0 && sx + x < WIDTH && (~sprite.priority || !scanlineRow[sx + x])) {
                    uint8_t color;

                    if (sprite.hFlip) {
                        color = m_tiles[sprite.tile][tileRow][7 - x];
                    } else {
                        color = m_tiles[sprite.tile][tileRow][x];
                    }
                    if (color) {
                        m_sprScreen.SetPixel(sx + offset, m_scanline, p[color]);
                    }

                    offset++;
                }
            }
        }
    }
}

