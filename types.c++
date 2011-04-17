#include "types.h"

#include "assembly.h"
#include "opcodes.h"
#include "noreturn.h"

#include <stdexcept>
#include <sstream>
#include <iostream>

using namespace opcode;

Type *Type::any = new (class _any: public Type {
  public:
    bool isConcrete() { return false; }
    unsigned int getSize() { typeResolutionIncomplete(); }
    bool canConvertTo(Type *) { return true; }
    void convertTo(Type *, Assembly &) { typeResolutionIncomplete(); }
    void compile(Assembly &) { typeResolutionIncomplete(); }

  private:
    void NORET typeResolutionIncomplete() {
      compileError("Type resolution incomplete: Element with every possible type remained.");
    }
});

Type *Type::none = new (class _none: public Type {
  public:
    bool isConcrete() { return false; }
    unsigned int getSize() { typeResolutionIncomplete(); }
    bool canConvertTo(Type *t) { return t == Type::none; }
    void convertTo(Type *, Assembly &) { typeResolutionIncomplete(); }
    void compile(Assembly &) { typeResolutionIncomplete(); }

  private:
    void NORET typeResolutionIncomplete() {
      compileError("Type resolution incomplete: Element without type remained.");
    }
});

Type *Type::boolean = new (class _boolean: public Type {
  public:
    bool isConcrete() { return true; }
    bool canConvertTo(Type *t) {
      // TODO: allow meaningful conversions
      return t == Type::none || t == this;
    }

    void convertTo(Type *t, Assembly &) {
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
    bool isConcrete() { return true; }
    bool canConvertTo(Type *t) {
      // TODO: allow meaningful conversions
      return t == Type::none || t == this;
    }

    void convertTo(Type *t, Assembly &) {
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
    bool isConcrete() { return true; }
    bool canConvertTo(Type *t) {
      // TODO: allow meaningful conversions
      return t == Type::none || t == this;
    }

    void convertTo(Type *t, Assembly &) {
      // TODO: allow meaningful conversions
      assert(t == this);
    }

    unsigned int getSize() { return 8; }

    void compile(Assembly &assembly) {
      assembly.add(move(static_cast<uint64_t>(0), rax()));
    }
});

bool TypeFunction::canConvertTo(Type *t) {
  if(t == Type::none) return true;

  auto ft = dynamic_cast<TypeFunction *>(t);
  if(!ft) return false;

  if(!returnType->canConvertTo(ft->getReturnType())) return false;
  if(ft->getArgumentCount() < argumentTypes.size()) return false;
  for(size_t i = 0; i < argumentTypes.size(); ++i) {
    if(!ft->getArgumentType(i)->canConvertTo(argumentTypes[i])) return false;
  }

  return true;
}

Type *Type::min(Type *a, Type *b) {
  if(a->canConvertTo(b)) return b;
  if(b->canConvertTo(a)) return a;

  return Type::none;
}

bool TypeFunction::operator == (Type &other) {
  TypeFunction *func = dynamic_cast<TypeFunction *>(&other);
  if(!func) return false;

  if(!!returnType != !!func->returnType) return false;
  if(returnType && func->returnType && *returnType != *func->returnType) return false;
  if(argumentTypes.size() != func->argumentTypes.size()) return false;

  for(size_t i = 0; i < argumentTypes.size(); ++i) {
    if(!!argumentTypes[i] != !!func->argumentTypes[i]) return false;
    if(argumentTypes[i] && func->argumentTypes[i] && *argumentTypes[i] != *func->argumentTypes[i]) return false;
    if(argumentRanks[i] != func->argumentRanks[i]) return false;
  }

  return true;
}

Type *TypeFunction::generate(Type *t) {
  TypeFunction *ret = new TypeFunction();

  for(size_t i = 0; i < argumentTypes.size(); ++i) {
    ret->addArgument(argumentTypes[i], argumentRanks[i]);
  }

  ret->setReturnType(t);

  return ret;
}

bool TypeFunction::isConcrete() {
  if(!returnType->isConcrete()) return false;
  
  for(auto i = argumentTypes.begin(); i != argumentTypes.end(); ++i) {
    if(!(*i)->isConcrete()) return false;
  }

  return true;
}

std::string TypeFunction::dump(int i) {
  std::ostringstream ret;
  ret << indent(i) << "TypeFunction\n"
    << indent(i + 2) << "Ret: " << (returnType? returnType->dump(i): "0x0") << "\n"
    << indent(i + 2) << "Args: " << argumentTypes.size();

  for(size_t j = 0; j < argumentTypes.size(); ++j) {
    ret << "\n" << indent(i + 2) << "Rank: " << argumentRanks[j] << "\n"
      << (argumentTypes[j]? argumentTypes[j]->dump(i + 2): "0x0");
  }

  return ret.str();
}

bool TypeTuple::operator == (Type &other) {
  TypeTuple *tuple = dynamic_cast<TypeTuple *>(&other);
  if(!tuple) return false;
  if(getTupleWidth() != tuple->getTupleWidth()) return false;

  for(size_t i = 0; i < getTupleWidth(); ++i) {
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

  for(size_t j = 0; j < i; ++j) {
    ret += elementTypes[j]->getSize();
  }

  return ret;
}

// TODO: conversion would be possible, if all elements can be converted
bool TypeTuple::canConvertTo(Type *t) {
  if(t == Type::none) return true;
  if(*this == *t) return true;
  
  if(auto tt = dynamic_cast<TypeTuple *>(t)) {
    if(tt->getTupleWidth() > getTupleWidth()) return false;

    for(size_t i = 0; i < tt->getTupleWidth(); ++i) {
      if(!getElementType(i)->canConvertTo(tt->getElementType(i))) return false;
    }

    return true;
  }

  return getElementType(0)->canConvertTo(t);
}

void TypeTuple::compile(Assembly &) {
  // TODO: initialize tuples
  assert(false);
}

bool TypeTuple::isConcrete() {
  for(auto i = elementTypes.begin(); i != elementTypes.end(); ++i) {
    if(!(*i)->isConcrete()) return false;
  }

  return true;
}

bool TypeDomained::canTakeDomainFrom(TypeDomained *other) {
  if(getArgumentCount() > other->getArgumentCount()) return false;

  for(size_t i = 0; i < getArgumentCount(); ++i) {
    if(!other->getArgumentType(i)->canConvertTo(getArgumentType(i))) return false;
  }

  return true;
}
