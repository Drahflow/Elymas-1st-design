#include "library.h"

#include "symboltable.h"
#include "types.h"

#include <iostream>
#include <sys/mman.h>

void Library::addStandardTypes(SymbolTable *st) {
  st->addType("s32", Type::sint32);
  st->addType("u64", Type::uint64);
}

void Library::addStandardFunctions(SymbolTable *st) {
  Symbol *var = st->addVar("putchar",
      (new TypeFunction())
        ->setReturnType(Type::sint32)
        ->addArgument(Type::sint32, 1));

  SymbolGlobalVariable *global = dynamic_cast<SymbolGlobalVariable *>(var);
  assert(global);

  *reinterpret_cast<void **>(global->getAddress()) =
    reinterpret_cast<void *>(&putchar);
}

// memory layout:
//   each of the memory pointers go to an area
//   first 8 byte of area go to first free object in that area (or 0)
//   each object starts with 8 byte length with the
//   lower three bits are not actually length but used for other things
//   every object is at least of size 8
//   bit 0 used for "seen" flag of garbage collector
//   bit 1+2 free so far
//   free pointers point on the length headers
//   language pointers point on the data area (obviously)
//   a free object uses the 8 minimal bytes for pointer to next free object
//   garbage sweep uses a terminating ~0 length object in each area
//   to know when to stop

struct Library::MemoryObject {
  uint64_t length;

  union {
    Library::MemoryObject *nextFree;
    char space[1];
  };
};

struct Library::MemoryBlock {
  MemoryObject *firstFree;
  MemoryObject firstObject;
};

std::vector<Library::MemoryBlock *> Library::memory;

void *Library::newObject(uint64_t size) {
  static const uint64_t blockSize = 1024 * 1024;
  assert(sizeof(MemoryObject) == 16);

  bool zeroSized = !size;
  if(zeroSized) size = 8;

  assert(!(size & 3));

  for(auto i = memory.rbegin(); i != memory.rend(); ++i) {
    MemoryObject **free = &(*i)->firstFree;

    while(*free) {
      uint64_t length = (*free)->length & ~3;
      if(size <= length && length <= size + 8) {
//        std::cerr << "using up existing object" << std::endl;
        MemoryObject *ret = *free;
        *free = (*free)->nextFree;

        ret->length = size;
        // lying here, but that's useful for arrays
        if(zeroSized) ret->length = 0;

        return ret->space;
      } else if(length > size + 8) {
//        std::cerr << "splitting old object (size: "
//          << length << ")" << std::endl;
        MemoryObject *ret = *free;
        *free = reinterpret_cast<MemoryObject *>(
            reinterpret_cast<uint8_t *>(*free) + size + 8);
        (*free)->length = length - size - 8;
        (*free)->nextFree = ret->nextFree;

        ret->length = size;
        // lying here, but that's useful for arrays
        if(zeroSized) ret->length = 0;

        return ret->space;
      } else {
//        std::cerr << "skipping small object (size: "
//          << length << ")" << std::endl;
        free = &(*free)->nextFree;
      }
    }
  }

  // if any other object is to fit, there need to be 32 bytes more
  if(size > blockSize - 32) {
//    std::cerr << "allocating new big block" << std::endl;
    // 8 byte pointer to first free element
    // 8 byte length header of requested object
    // 8 byte zero filled end marker of block
    MemoryBlock *block =
      reinterpret_cast<MemoryBlock *>(
          mmap(0, 24 + size, PROT_EXEC | PROT_READ | PROT_WRITE,
            MAP_PRIVATE | MAP_ANONYMOUS, -1, 0));
    assert(block);
    memory.push_back(block);

    block->firstFree = 0;
    block->firstObject.length = size;
    *reinterpret_cast<uint64_t *>(
        reinterpret_cast<uint8_t *>(block) + 16 + size) = ~0;

    return block->firstObject.space;
  } else {
//    std::cerr << "allocating new usual block" << std::endl;
    MemoryBlock *block =
      reinterpret_cast<MemoryBlock *>(
          mmap(0, blockSize, PROT_EXEC | PROT_READ | PROT_WRITE,
            MAP_PRIVATE | MAP_ANONYMOUS, -1, 0));
    assert(block);
    memory.push_back(block);

    MemoryObject *free = reinterpret_cast<MemoryObject *>(
        reinterpret_cast<uint8_t *>(block) + 16 + size);

    block->firstFree = free;
    block->firstObject.length = size;

    free->length = blockSize - size - 32;
    free->nextFree = 0;
    *reinterpret_cast<uint64_t *>(
        reinterpret_cast<uint8_t *>(block) + blockSize - 8) = ~0;

    return block->firstObject.space;
  }
}

void Library::dumpMemory() {
  for(auto i = memory.rbegin(); i != memory.rend(); ++i) {
    std::cout << "--- MEMORY SEGMENT ---" << std::endl;
    std::cout << *i << std::endl;
    std::cout << "Free List: " << std::endl;
    MemoryObject *free = (*i)->firstFree;
    while(free) {
      std::cout << free << " (size: " << free->length << ")" << std::endl;
      free = free->nextFree;
    }

    std::cout << "Block List: " << std::endl;
    MemoryObject *block = &(*i)->firstObject;
    while((block->length & ~3) != ~3) {
      std::cout << block << " (size: " << block->length << ")" << std::endl;
      uint64_t length = block->length & ~3;
      if(!length) length = 8;

      block = reinterpret_cast<MemoryObject *>(
          reinterpret_cast<uint8_t *>(block) + length + 8);
    }
  }
}

void Library::deleteObject(void *) {
  // zero sized objects are lying, remember that
  assert(false);
}
