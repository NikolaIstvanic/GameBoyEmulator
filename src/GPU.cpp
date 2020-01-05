#include <cstring>
#include <iostream>

#include "GameBoy.hpp"
#include "GPU.hpp"

GPU::GPU() {
    colorPalette = {{
        olc::Pixel(155, 188, 15), olc::Pixel(139, 172, 15),
        olc::Pixel(48, 98, 48), olc::Pixel(15, 56, 15)
    }};
}

void GPU::reset() {
    scanline = 0;
    scrollX = 0;
    scrollY = 0;
    clocks = 0;
    state = HBLANK;
    lcdControl = 0x00;
    lcdStatus = 0x00;
    lyCompare = 0x00;

    bg[0] = colorPalette[0];
    bg[1] = colorPalette[1];
    bg[2] = colorPalette[2];
    bg[3] = colorPalette[3];
    spritePalette[0][0] = colorPalette[0];
    spritePalette[0][1] = colorPalette[1];
    spritePalette[0][2] = colorPalette[2];
    spritePalette[0][3] = colorPalette[3];
    spritePalette[1][0] = colorPalette[0];
    spritePalette[1][1] = colorPalette[1];
    spritePalette[1][2] = colorPalette[2];
    spritePalette[1][3] = colorPalette[3];
    memset(tiles, 0x00, sizeof(tiles));
}

olc::Sprite& GPU::getScreen() { return sprScreen; }

void GPU::step(uint8_t cpuCycles) {
    clocks += cpuCycles;

    switch (state) {
        case HBLANK:
            if (clocks >= 204) {
                scanline++;
                if (scanline == 144) {
                    // request vblank interrupt
                    gb->cpu.intFlags |= INT_VBLANK;
                    state = VBLANK;
                    lcdStatus &= ~0x1;
                    lcdStatus |= 0x2;
                    frameDone = true;
                } else {
                    state = OAM;
                    lcdStatus |= 0x2;
                    lcdStatus &= ~0x1;
                }
                clocks -= 204;
            }
            break;

        case VBLANK:
            if (clocks >= 456) {
                scanline++;
                if (scanline > 153) {
                    scanline = 0;
                    lcdStatus |= 0x2;
                    lcdStatus &= ~0x1;
                    state = OAM;
                }
                clocks -= 456;
            }
            break;

        case OAM:
            if (clocks >= 80) {
                clocks -= 80;
                lcdStatus |= 0x2;
                lcdStatus &= ~0x1;
                state = VRAM;
            }
            break;

        case VRAM:
            if (clocks >= 172) {
                state = HBLANK;
                updateScreen();
                clocks -= 172;
                if (lcdStatus & 0x08) {
                    gb->cpu.intFlags |= INT_LCDC;
                }
                uint8_t lyCoincidenceInt = lcdStatus & 0x40;
                uint8_t lyCoincidence = lyCompare == scanline;
                if (lyCoincidenceInt && lyCoincidence) {
                    gb->cpu.intFlags |= INT_LCDC;
                }
                lyCoincidence ? lcdStatus |= 0x4 : lcdStatus &= ~0x4;
                lcdStatus &= ~0x2;
                lcdStatus &= ~0x1;
            }
            break;
    }
}

void GPU::updateScreen() {
    uint16_t mapOffset = ((lcdControl & TILEMAP) ? 0x1C00 : 0x1800)
        + ((((scanline + scrollY) & 0xFF) >> 3) << 5);
    uint8_t lineOffset = scrollX >> 3;
    uint8_t x = scrollX & 0x07;
    uint8_t y = (scanline + scrollY) & 0x07;
    uint16_t tile = (uint16_t) (gb->read8(0x8000 + mapOffset + lineOffset));
    uint16_t scanlineRow[WIDTH];

    for (uint16_t i = 0; i < WIDTH; i++) {
        uint8_t color = tiles[tile][y][x];
        scanlineRow[i] = color;
        sprScreen.SetPixel(i, scanline, bg[color]);
        if (++x == 8) {
            x = 0;
            lineOffset = (lineOffset + 1) & 0x1F;
            tile = (uint16_t) gb->read8(0x8000 + mapOffset + lineOffset);
        }
    }

    for (uint8_t i = 0; i < 40; i++) {
        struct gpuSprite sprite = ((struct gpuSprite*) (gb->RAM.data() + 0xFE00))[i];
        uint16_t sx = sprite.x - 8;
        uint16_t sy = sprite.y - 16;

        if (sy <= scanline && (sy + 8) > scanline) {
            std::array<olc::Pixel, 4> p = spritePalette[sprite.palette];
            uint8_t tileRow;

            if (sprite.vFlip) {
                tileRow = 7 - (scanline - sy);
            } else {
                tileRow = scanline - sy;
            }

            for (int x = 0; x < 8; x++) {
                if (sx + x >= 0 && sx + x < WIDTH && (~sprite.priority || !scanlineRow[sx + x])) {
                    uint8_t color;

                    if (sprite.hFlip) {
                        color = tiles[sprite.tile][tileRow][7 - x];
                    } else {
                        color = tiles[sprite.tile][tileRow][x];
                    }
                    if (color) {
                        sprScreen.SetPixel(sx + i, scanline, p[color]);
                    }
                }
            }
        }
    }
}

