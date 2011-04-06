#include "main.h"

#include <memory>
#include <string.h>

static void help() {
  std::cerr << "Usage: ./elymas [--debug] <input>" << std::endl;
}

int main(int argc, char **argv) {
  bool debug = false;

  int pos = 1;
  if(argc < pos) { help(); return 1; }
  if(!strcmp(argv[pos], "--debug")) {
    debug = true;
    ++pos;
  }
  if(argc < pos) { help(); return 1; }

  Main mainloop(debug);
  mainloop.parse(argv[pos]);
}
