#ifndef ASSEMBLY_H
#define ASSEMBLY_H

#include "opcodes.h"

#include <vector>

class Assembly {
  private:
    std::vector<Opcode *> opcodes;

  public:
    Assembly() { };

    void *assemble();

    void add(Opcode *op) {
      opcodes.push_back(op);
    }

    Label *label();
};

#endif
