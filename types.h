#ifndef TYPES_H
#define TYPES_H

#include "treenodes.h"

#include <vector>
#include <cassert>
#include <string>
#include <stdexcept>

class Assembly;

class Type: public NodeExpr {
  public:
    virtual ~Type() { }
    virtual unsigned int getSize() = 0;
    virtual bool canConvertTo(Type *) = 0;
    virtual void convertTo(Type *other, Assembly &) {
      assert(*this == *other);
    }

    virtual bool operator == (Type &other) {
      return this == &other;
    }

    bool operator != (Type &other) {
      return !(*this == other);
    }

    static Type *any;
    static Type *none;
    static Type *sint32;
    static Type *uint64;
    static Type *boolean;

    void rewriteDeclarations(SymbolTable *, NodeExpr **) { }
    void resolveSymbols(SymbolTable *) { assert(false); }
    void rewriteFunctionApplications(NodeExpr **) { assert(false); }
    void assignUnresolvedTypes(Type *) { assert(false); }
    void compile(Assembly &) = 0;
    void compileL(Assembly &) { assert(false); }
    Type *getType() { assert(false); } // TODO: add type "Type" one day
};

class TypeTuple: public Type {
  public:
    unsigned int getTupleWidth() {
      return elementTypes.size();
    }

    virtual Type *getElementType(unsigned int i) {
      return elementTypes[i];
    }

    virtual unsigned int getElementOffset(unsigned int i);

    TypeTuple *addElementType(Type *);
    unsigned int getSize();
    unsigned int getHeapSize();
    bool canConvertTo(Type *);

    bool operator == (Type &);
    std::string dump(int index);

    void compile(Assembly &);

  private:
    std::vector<Type *> elementTypes;
};

class TypeDomained: public Type {
  public:
    virtual Type *getReturnType() = 0;
    virtual Type *generate(Type *ret) = 0;

    virtual unsigned int getArgumentCount() = 0;
    virtual int getArgumentRank(unsigned int i) = 0;
    virtual Type *getArgumentType(unsigned int i) = 0;

    bool canTakeDomainFrom(TypeDomained *);
};

class TypeLoopable: public TypeDomained {
  public:
    virtual Type *getReturnType() {
      return innerType;
    }

    // takes container in %rax, creates target structure with same key set in %rax
    virtual void loopGenerate(Assembly &, Type *result) = 0;

    // takes container in %rax, returns first key in %rcx
    // creates necessary looping label, jumps to end if loop is completed
    virtual void *loopBegin(Assembly &, Type *result) = 0;
    // takes target container in %rdx, current key in %rcx, data in %rax, copies data into %rcx bucket of target container
    virtual void loopSaveResult(Assembly &, Type *result) = 0;
    // takes container in %rax, current key in %rcx, returns next key in %rcx
    virtual void loopStep(Assembly &, Type *result) = 0;
    // takes container in %rax, foreign key in %rcx, returns correct local key in %rcx
    virtual void loopAdjustKey(Assembly &, Type *result) = 0;
    // takes container in %rax, jumps to start label
    virtual void loopEnd(Assembly &, Type *result, void *) = 0;

    // take index in %rcx, object in %rax
    // return result in %rax
    virtual void dereference(Assembly &) = 0;

    virtual unsigned int getArgumentCount() { return 1; }

  protected:
    TypeLoopable(Type *innerType): innerType(innerType) { }

    Type *innerType;
};

class TypeFunction: public TypeDomained {
  public:
    TypeFunction() { }

    virtual unsigned int getArgumentCount() {
      return argumentTypes.size();
    }

    virtual int getArgumentRank(unsigned int i) {
      return argumentRanks[i];
    }

    virtual Type *getArgumentType(unsigned int i) {
      return argumentTypes[i];
    }

    TypeFunction *setReturnType(Type *type) { returnType = type; return this; }
    Type *getReturnType() { return returnType; }

    TypeFunction *addArgument(Type *type, int rank) {
      assert(!dynamic_cast<TypeTuple *>(type));

      argumentTypes.push_back(type);
      argumentRanks.push_back(rank);
      return this;
    }

    bool canConvertTo(Type *);

    unsigned int getSize() {
      return 8;
    }

    void compile(Assembly &) { assert(false); } // TODO: initialize function vars

    bool operator == (Type &);
    std::string dump(int index);

    Type *generate(Type *ret);

  private:
    std::vector<Type *> argumentTypes;
    std::vector<int> argumentRanks;
    Type *returnType;
};

#endif
