#include "assembly.h"

#include <string.h>
#include <sys/mman.h>

using namespace opcode;

void *Assembly::assemble() {
  opcodes.push_back(retnear());

  unsigned long long size = 0;
  for(auto i = opcodes.begin(); i != opcodes.end(); ++i) {
    size += (*i)->size(size);
  }

  unsigned char *binary = reinterpret_cast<unsigned char *>(
    mmap(0, size, PROT_EXEC | PROT_READ | PROT_WRITE,
        MAP_PRIVATE | MAP_ANONYMOUS, -1, 0));

  size = 0;
  for(auto i = opcodes.begin(); i != opcodes.end(); ++i) {
    (*i)->write(binary + size);
    size += (*i)->size(size);
  }

  return binary;
}

Label *Assembly::label() {
  return new Label();
}
