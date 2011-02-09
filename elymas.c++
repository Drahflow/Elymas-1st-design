#include "main.h"

#include <memory>

int main(int argc, char **argv) {
  if(argc != 2) {
    std::cerr << "Usage: ./elymas <input>" << std::endl;
  }

  Main mainloop;
  mainloop.parse(argv[1]);
}
