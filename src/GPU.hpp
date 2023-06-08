#pragma once

#include <array>
#include <cstdint>
#include <memory>

#include "olcPixelGameEngine.h"

#define WIDTH  160
#define HEIGHT 144

class GameBoy;

class GPU {
public:
    GPU();
    ~GPU();

    void reset();
    void connectGameBoy(GameBoy* gb);
    olc::Sprite& getScreen();
    void step(uint8_t cpuCycles);
    void updateTile(uint16_t addr, uint8_t data);
    void updateScreen();

    enum {
        HBLANK,
        VBLANK,
        OAM,
        VRAM
    } m_state;

    bool m_frameDone = false;
    uint16_t m_clocks = 0;
    uint8_t m_scanline = 0x00;
    uint8_t m_control = 0x00;
    uint8_t m_scrollX = 0;
    uint8_t m_scrollY = 0;

    std::array<olc::Pixel, 4> m_bg;
    std::array<std::array<olc::Pixel, 4>, 2> m_spritePalette;
    std::array<olc::Pixel, 4> m_colorPalette;
    uint8_t m_tiles[384][8][8];

    uint8_t m_lcdControl = 0x00;
    uint8_t m_lcdStatus = 0x00;
    uint8_t m_lyCompare = 0x00;

private:
    struct gpuSprite {
        uint8_t y;
        uint8_t x;
        uint8_t tile;
        uint8_t priority : 1;
        uint8_t vFlip : 1;
        uint8_t hFlip : 1;
        uint8_t palette : 1;
    };

    GameBoy* m_gb = nullptr;
    olc::Sprite m_sprScreen = olc::Sprite(WIDTH, HEIGHT);
};

