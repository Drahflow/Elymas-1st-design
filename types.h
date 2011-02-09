#ifndef TYPES_H
#define TYPES_H

#include "treenodes.h"

#include <vector>
#include <cassert>
#include <string>

class Assembly;

class Type: public NodeExpr {
  public:
    virtual ~Type() { }
    virtual unsigned int getTupleWidth() { return 1; }
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

    static Type *all;
    static Type *sint32;
    static Type *uint64;

    void rewriteDeclarations(SymbolTable *, NodeExpr **) { }
    void resolveSymbols(SymbolTable *) { assert(false); }
    void rewriteFunctionApplications(NodeExpr **) { assert(false); }
    void compile(Assembly &) = 0;
    void compileL(Assembly &) { assert(false); }
    Type *getType() { assert(false); } // TODO: add type "Type" one day
};

class TypeTuple: public Type {
  public:
    virtual unsigned int getTupleWidth() {
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

class TypeLoopable: public Type {
  public:
    virtual Type *getInnerType() {
      return innerType;
    }

    virtual void *loopBegin(Assembly &, Type *result) = 0;
    virtual void loopStep(Assembly &, Type *result, void *) = 0;
    virtual void loopStepSecondary(Assembly &, Type *result, void *) = 0;
    virtual void loopEnd(Assembly &, Type *result, void *) = 0;

    virtual Type *generate(Type *inner) = 0;

  protected:
    TypeLoopable(Type *innerType): innerType(innerType) { }

    Type *innerType;
};

class TypeFunction: public Type {
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

  private:
    std::vector<Type *> argumentTypes;
    std::vector<int> argumentRanks;
    Type *returnType;
};

class NodeExprFunction;

class TypeFunctionSet: public Type {
  public:
    virtual NodeExprFunction *get(Type *argumentType) = 0;
};

#endif
