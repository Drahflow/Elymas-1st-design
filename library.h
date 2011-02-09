#ifndef LIBRARY_H
#define LIBRARY_H

#include <stdint.h>
#include <vector>

class SymbolTable;

class Library {
  public:
    static void addStandardTypes(SymbolTable *);
    static void addStandardFunctions(SymbolTable *);
    static void *newObject(uint64_t size);
    static void deleteObject(void *);
    static void dumpMemory();

  private:
    struct MemoryObject;
    struct MemoryBlock;

    static std::vector<MemoryBlock *> memory;
};

#endif
