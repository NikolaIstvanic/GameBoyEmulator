#pragma once

#include <array>
#include <cstdint>
#include <memory>

#include "olcPixelGameEngine.h"

#define WIDTH 160
#define HEIGHT 144

#define SPRENABLE 0x02
#define TILEMAP 0x08

class GameBoy;

class GPU {
    public:
        GPU();
        ~GPU() = default;
        void reset();
        void connectGameBoy(GameBoy* g) { gb = g; }
        olc::Sprite& getScreen();
        void step(uint8_t cpuCycles);
        void updateTile(uint16_t addr, uint8_t data);
        void updateScreen();

        enum {
            HBLANK,
            VBLANK,
            OAM,
            VRAM
        } state;

        bool frameDone = false;
        uint16_t clocks = 0;
        uint8_t scanline = 0x00;
        uint8_t control = 0x00;
        uint8_t scrollX = 0;
        uint8_t scrollY = 0;

        std::array<olc::Pixel, 4> bg;
        std::array<std::array<olc::Pixel, 4>, 2> spritePalette;
        std::array<olc::Pixel, 4> colorPalette;
        uint8_t tiles[384][8][8];

        uint8_t lcdControl = 0x00;
        uint8_t lcdStatus = 0x00;
        uint8_t lyCompare = 0x00;

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

        GameBoy* gb = nullptr;

        olc::Sprite sprScreen = olc::Sprite(WIDTH, HEIGHT);
};

