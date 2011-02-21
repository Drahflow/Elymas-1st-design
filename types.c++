#include "types.h"

#include "assembly.h"
#include "opcodes.h"

#include <stdexcept>
#include <sstream>
#include <iostream>

using namespace opcode;

Type *Type::all = new (class _all: public Type {
  public:
    bool canConvertTo(Type *t) {
      return true;
    }

    void convertTo(Type *t, Assembly &assembly) {
      throw std::runtime_error("trying to convert unbound type to anything");
    }

    unsigned int getSize() { assert(false); }
    void compile(Assembly &assembly) { assert(false); }
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
  return false;
}

bool TypeFunction::operator == (Type &other) {
  TypeFunction *func = dynamic_cast<TypeFunction *>(&other);
  if(!func) return false;

  if(*returnType != *func->returnType) return false;
  if(argumentTypes.size() != func->argumentTypes.size()) return false;

  for(int i = 0; i < argumentTypes.size(); ++i) {
    if(*argumentTypes[i] != *func->argumentTypes[i]) return false;
    if(argumentRanks[i] != func->argumentRanks[i]) return false;
  }

  return true;
}

std::string TypeFunction::dump(int i) {
  std::ostringstream ret;
  ret << indent(i) << "TypeFunction\n" << returnType->dump(i + 2);

  for(int i = 0; i < argumentTypes.size(); ++i) {
    ret << "\n" << indent(i + 2) << "Rank: " << argumentRanks[i] << "\n"
      << argumentTypes[i]->dump(i + 2);
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
  unsigned int ret;

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

bool TypeFunctionSet::canConvertTo(Type *other) {
  TypeFunction *func = dynamic_cast<TypeFunction *>(other);
  if(!func) return false;

  TypeTuple *argumentType = new TypeTuple();
  for(int i = 0; i < func->getArgumentCount(); ++i) {
    argumentType->addElementType(func->getArgumentType(i));
  }

  NodeExprFunction *function = get(argumentType);
  if(!function) return false;

  TypeFunction *resolvedFunc = dynamic_cast<TypeFunction *>(function->getType());
  if(!resolvedFunc) return false;

  if(!resolvedFunc->getReturnType()->canConvertTo(func->getReturnType())) return false;

  return true;
}

void TypeFunctionSet::convertTo(Type *other, Assembly &assert) {
  // TODO
  assert(false);
}

NodeExprFunction *TypeNodeBackedFunctionSet::get(Type *t) {
  return node->assignType(t);
}
