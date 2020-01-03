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
        GameBoyEmulator(std::string romPath) {
            sAppName = "GameBoy Emulator";
            this->romPath = romPath;
        }

    private:
        GameBoy gb;
        float residualTime = 0.0f;
        std::string romPath;

        bool OnUserCreate() override {
            std::ifstream ifs;
            ifs.open(romPath, std::ifstream::binary);

            if (ifs.is_open()) {
                ifs.read((char*) gb.RAM.data(), SIZE_CART);
                gb.reset();
                ifs.close();
                return true;
            }
            std::cout << "Error reading file" << std::endl;
            return false;
        }

        bool OnUserUpdate(float elapsedTime) override {
            gb.keys.a = GetKey(olc::Key::K).bHeld ? 0 : 1;
            gb.keys.b = GetKey(olc::Key::J).bHeld ? 0 : 1;
            gb.keys.select = GetKey(olc::Key::I).bHeld ? 0 : 1;
            gb.keys.start = GetKey(olc::Key::U).bHeld ? 0 : 1;
            gb.keys.right = GetKey(olc::Key::D).bHeld ? 0 : 1;
            gb.keys.left = GetKey(olc::Key::A).bHeld ? 0 : 1;
            gb.keys.up = GetKey(olc::Key::W).bHeld ? 0 : 1;
            gb.keys.down = GetKey(olc::Key::S).bHeld ? 0 : 1;

            if (GetKey(olc::Key::R).bPressed) {
                gb.reset();
            }
            if (GetKey(olc::Key::ESCAPE).bPressed) {
                return false;
            }

#if 0
            if (GetKey(olc::Key::SPACE).bPressed) {
                for (int i = 0; i < 20; i++) {
                    do { gb.step(); } while (gb.cpu.cycles != 0);
                    do { gb.step(); } while (gb.cpu.cycles == 0);
                }
            }
            if (GetKey(olc::Key::S).bPressed) {
                do { gb.step(); } while (gb.cpu.cycles != 0);
                do { gb.step(); } while (gb.cpu.cycles == 0);
            }
            if (GetKey(olc::Key::F).bPressed) {
                do { gb.step(); } while (!gb.gpu.frameDone);
                gb.gpu.frameDone = false;
            }
#endif

#if 1
            if (residualTime > 0.0) {
                residualTime -= elapsedTime;
            } else {
                residualTime += (1.0f / 60.0f) - elapsedTime;
                do { gb.step(); } while (!gb.gpu.frameDone);
                gb.gpu.frameDone = false;
            }
#endif

            DrawSprite(0, 0, &gb.gpu.getScreen(), 1);

            return true;
        }
};

int main(int argc, char* argv[]) {
    if (argc > 1) {
        GameBoyEmulator emu(argv[1]);
        if (emu.Construct(WIDTH, HEIGHT, 2, 2)) {
            emu.Start();
        }
    } else {
        std::cout << "Provide path to ROM" << std::endl;
    }
    return 0;
}

