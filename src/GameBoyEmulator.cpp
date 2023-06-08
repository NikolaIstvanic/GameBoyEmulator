/**
 *  					 __________________________
 *  					|OFFo oON                  |
 *  					| .----------------------. |
 *  					| |  .----------------.  | |
 *  					| |  |                |  | |
 *  					| |))|                |  | |
 *  					| |  |                |  | |
 *  					| |  |                |  | |
 *  					| |  |                |  | |
 *  					| |  |                |  | |
 *  					| |  |                |  | |
 *  					| |  '----------------'  | |
 *  					| |__GAME BOY____________/ |
 *  					|          ________        |
 *  					|    .    (Nintendo)       |
 *  					|  _| |_   """"""""   .-.  |
 *  					|-[_   _]-       .-. (   ) |
 *  					|   |_|         (   ) '-'  |
 *  					|    '           '-'   A   |
 *  					|                 B        |
 *  					|          ___   ___       |
 *  					|         (___) (___)  ,., |
 *  					|        select start ;:;: |
 *  					|                    ,;:;' /
 *  					|                   ,:;:'.'
 *  					'-----------------------`
 *
 */
#include <fstream>
#include <iostream>
#include <string>

#include "GameBoy.hpp"

#define OLC_PGE_APPLICATION
#include "olcPixelGameEngine.h"

#define SIZE_CART 0x8000

class GameBoyEmulator : public olc::PixelGameEngine {
public:
    GameBoyEmulator(const std::string& romPath) : m_romPath{romPath} {
        sAppName = "GameBoy Emulator";
    }

private:
    bool OnUserCreate() override {
        std::ifstream ifs;
        ifs.open(m_romPath, std::ifstream::binary);

        if (ifs.is_open()) {
            ifs.read((char*) m_gb.m_ram.data(), SIZE_CART);
            m_gb.reset();
            ifs.close();
            return true;
        }
        std::cerr << "Error reading file" << std::endl;
        return false;
    }

    bool OnUserUpdate(float elapsedTime) override {
        m_gb.m_keys.a = GetKey(olc::Key::K).bHeld ? 0 : 1;
        m_gb.m_keys.b = GetKey(olc::Key::J).bHeld ? 0 : 1;
        m_gb.m_keys.select = GetKey(olc::Key::I).bHeld ? 0 : 1;
        m_gb.m_keys.start = GetKey(olc::Key::U).bHeld ? 0 : 1;
        m_gb.m_keys.right = GetKey(olc::Key::D).bHeld ? 0 : 1;
        m_gb.m_keys.left = GetKey(olc::Key::A).bHeld ? 0 : 1;
        m_gb.m_keys.up = GetKey(olc::Key::W).bHeld ? 0 : 1;
        m_gb.m_keys.down = GetKey(olc::Key::S).bHeld ? 0 : 1;

        if (GetKey(olc::Key::R).bPressed) {
            m_gb.reset();
        }
        if (GetKey(olc::Key::ESCAPE).bPressed) {
            return false;
        }

#if 0
        if (GetKey(olc::Key::SPACE).bPressed) {
            for (int i = 0; i < 20; i++) {
                gb.step();
            }
        }
        if (GetKey(olc::Key::S).bPressed) {
            gb.step();
        }
        if (GetKey(olc::Key::F).bPressed) {
            do { gb.step(); } while (!gb.gpu.frameDone);
            gb.gpu.frameDone = false;
        }
#endif

#if 1
        if (m_residualTime > 0.0) {
            m_residualTime -= elapsedTime;
        } else {
            m_residualTime += (1.0f / 60.0f) - elapsedTime;

            do {
                m_gb.step();
            } while (!m_gb.m_gpu.m_frameDone);

            m_gb.m_gpu.m_frameDone = false;
        }
#endif

        DrawSprite(0, 0, &m_gb.m_gpu.getScreen(), 1);
        return true;
    }

    GameBoy m_gb;
    float m_residualTime = 0.0f;
    std::string m_romPath;
};

int main(int argc, char* argv[]) {
    if (argc > 1) {
        GameBoyEmulator emu{argv[1]};

        if (emu.Construct(WIDTH, HEIGHT, 2, 2)) {
            emu.Start();
        }
    } else {
        std::cerr << "Provide path to ROM" << std::endl;
    }

    return 0;
}

