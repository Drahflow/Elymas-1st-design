#include "types.h"

#include "assembly.h"
#include "opcodes.h"

#include <stdexcept>
#include <sstream>
#include <iostream>

using namespace opcode;

Type *Type::any = new (class _any: public Type {
  public:
    unsigned int getSize() { typeResolutionIncomplete(); }
    bool canConvertTo(Type *) { return true; }
    void convertTo(Type *t, Assembly &assembly) { typeResolutionIncomplete(); }
    void compile(Assembly &assembly) { typeResolutionIncomplete(); }

  private:
    void typeResolutionIncomplete() {
      compileError("Type resolution incomplete: Element with every possible type remained.");
    }
});

Type *Type::none = new (class _none: public Type {
  public:
    unsigned int getSize() { typeResolutionIncomplete(); }
    bool canConvertTo(Type *) { return false; }
    void convertTo(Type *t, Assembly &assembly) { typeResolutionIncomplete(); }
    void compile(Assembly &assembly) { typeResolutionIncomplete(); }

  private:
    void typeResolutionIncomplete() {
      compileError("Type resolution incomplete: Element without type remained.");
    }
});

Type *Type::boolean = new (class _boolean: public Type {
  public:
    bool canConvertTo(Type *t) {
      // TODO: allow meaningful conversions
      return t == this;
    }

    void convertTo(Type *t, Assembly &assembly) {
      // TODO: allow meaningful conversions
      assert(t == this);
    }

    // TODO: once arrays can handly non-8-multiplies, fix this
    unsigned int getSize() { return 8; }

    void compile(Assembly &assembly) {
      assembly.add(move(static_cast<uint64_t>(0), rax()));
    }
});

Type *Type::sint32 = new (class _sint32: public Type {
  public:
    bool canConvertTo(Type *t) {
      // TODO: allow meaningful conversions
      return t == this;
    }

    void convertTo(Type *t, Assembly &assembly) {
      // TODO: allow meaningful conversions
      assert(t == this);
    }

    // TODO: once arrays can handly non-8-multiplies, fix this
    unsigned int getSize() { return 8; }

    void compile(Assembly &assembly) {
      assembly.add(move(static_cast<uint64_t>(0), rax()));
    }
});

Type *Type::uint64 = new (class _uint64: public Type {
  public:
    bool canConvertTo(Type *t) {
      // TODO: allow meaningful conversions
      return t == this;
    }

    void convertTo(Type *t, Assembly &assembly) {
      // TODO: allow meaningful conversions
      assert(t == this);
    }

    unsigned int getSize() { return 8; }

    void compile(Assembly &assembly) {
      assembly.add(move(static_cast<uint64_t>(0), rax()));
    }
});

bool TypeFunction::canConvertTo(Type *super) {
  // TODO: allow meaningful conversions
  return *this == *super;
}

bool TypeFunction::operator == (Type &other) {
  TypeFunction *func = dynamic_cast<TypeFunction *>(&other);
  if(!func) return false;

  if(!!returnType != !!func->returnType) return false;
  if(returnType && func->returnType && *returnType != *func->returnType) return false;
  if(argumentTypes.size() != func->argumentTypes.size()) return false;

  for(int i = 0; i < argumentTypes.size(); ++i) {
    if(!!argumentTypes[i] != !!func->argumentTypes[i]) return false;
    if(argumentTypes[i] && func->argumentTypes[i] && *argumentTypes[i] != *func->argumentTypes[i]) return false;
    if(argumentRanks[i] != func->argumentRanks[i]) return false;
  }

  return true;
}

Type *TypeFunction::generate(Type *ret) {
  // TODO generate clone with fixed return type
  assert(false);
}

std::string TypeFunction::dump(int i) {
  std::ostringstream ret;
  ret << indent(i) << "TypeFunction\n"
    << indent(i + 2) << "Ret: " << (returnType? returnType->dump(i): "0x0") << "\n"
    << indent(i + 2) << "Args: " << argumentTypes.size();

  for(int j = 0; j < argumentTypes.size(); ++j) {
    ret << "\n" << indent(i + 2) << "Rank: " << argumentRanks[j] << "\n"
      << (argumentTypes[j]? argumentTypes[j]->dump(i + 2): "0x0");
  }

  return ret.str();
}

bool TypeTuple::operator == (Type &other) {
  TypeTuple *tuple = dynamic_cast<TypeTuple *>(&other);
  if(!tuple) return false;
  if(getTupleWidth() != tuple->getTupleWidth()) return false;

  for(int i = 0; i < getTupleWidth(); ++i) {
    if(*getElementType(i) != *tuple->getElementType(i)) return false;
  }

  return true;
}

std::string TypeTuple::dump(int i) {
  std::string ret = indent(i) + "TypeTuple";

  for(auto j = elementTypes.begin(); j != elementTypes.end(); ++j) {
    ret += "\n" + (*j)->dump(i + 2);
  }

  return ret;
}

TypeTuple *TypeTuple::addElementType(Type *type) {
  elementTypes.push_back(type);

  return this;
}

unsigned int TypeTuple::getSize() {
  return 8;
}

unsigned int TypeTuple::getHeapSize() {
  unsigned int ret = 0;

  for(auto i = elementTypes.begin(); i != elementTypes.end(); ++i) {
    ret += (*i)->getSize();
  }

  return ret;
}

unsigned int TypeTuple::getElementOffset(unsigned int i) {
  unsigned int ret;

  for(int j = 0; j < i; ++j) {
    ret += elementTypes[j]->getSize();
  }

  return ret;
}

bool TypeTuple::canConvertTo(Type *other) {
  // TODO: conversion would be possible, if all elements can be converted
  return *this == *other;
}

void TypeTuple::compile(Assembly &assembly) {
  // TODO: initialize tuples
  assert(false);
}

bool TypeDomained::canTakeDomainFrom(TypeDomained *other) {
  if(getArgumentCount() > other->getArgumentCount()) return false;

  for(int i = 0; i < getArgumentCount(); ++i) {
    if(!other->getArgumentType(i)->canConvertTo(getArgumentType(i))) return false;
  }

  return true;
}
