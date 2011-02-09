#ifndef SYMBOLTABLE_H
#define SYMBOLTABLE_H

#include <map>
#include <vector>
#include <string>
#include <limits>

class Type;

class Symbol {
  public:
    virtual ~Symbol() { }
};

class SymbolVariable: public Symbol {
  public:
    SymbolVariable(): type(0) { }

    Type *getType() const { return type; }
    SymbolVariable *setType(Type *t) { type = t; return this; }

  private:
    Type *type;
};

class SymbolGlobalVariable: public SymbolVariable {
  public:
    SymbolGlobalVariable(): SymbolVariable(), address(0) { }

    void *getAddress() const { return address; }
    SymbolGlobalVariable *setAddress(void *addr) { address = addr; return this; }

    SymbolGlobalVariable *setType(Type *t) {
      SymbolVariable::setType(t);
      return this;
    }

  private:
    void *address;
};

class SymbolLocalVariable: public SymbolVariable {
  public:
    static int32_t INVALID;

    SymbolLocalVariable(): SymbolVariable(), stackOffset(INVALID) { }

    SymbolLocalVariable *setType(Type *t) {
      SymbolVariable::setType(t);
      return this;
    }

    int32_t getStackOffset() const { return stackOffset; }
    SymbolLocalVariable *setStackOffset(int32_t offset) { stackOffset = offset; return this; }

  private:
    int32_t stackOffset;
};

class SymbolClosureVariable: public SymbolVariable {
  public:
    static int32_t INVALID;

    SymbolClosureVariable(): SymbolVariable(), heapOffset(INVALID) { }

    SymbolClosureVariable *setType(Type *t) {
      SymbolVariable::setType(t);
      return this;
    }

    SymbolVariable *getParentSymbol() { return parentSymbol; }
    SymbolClosureVariable *setParentSymbol(SymbolVariable *parent) {
      parentSymbol = parent;
      return this;
    }

    int32_t getHeapOffset() const { return heapOffset; }
    SymbolClosureVariable *setHeapOffset(int32_t offset) { heapOffset = offset; return this; }

  private:
    int32_t heapOffset;
    SymbolVariable *parentSymbol;
};

class SymbolType: public Symbol {
  public:
    SymbolType(): type(0) { }

    Type *getType() const { return type; }
    SymbolType *setType(Type *t) { type = t; return this; }

  private:
    Type *type;
};

class SymbolTable {
  public:
    virtual Symbol *resolve(const std::string &);
    virtual Symbol *addVar(const std::string &, Type *) = 0;
    virtual Symbol *addType(const std::string &, Type *) = 0;

    const std::map<std::string, Symbol *> &getSymbols() const { return symbols; }

  protected:
    Symbol *addSymbol(const std::string &, Symbol *);

    std::map<std::string, Symbol *> symbols;
    std::vector<SymbolTable *> searchPath;
};

class SymbolTableGlobal: public SymbolTable {
  public:
    Symbol *addVar(const std::string &, Type *);
    Symbol *addType(const std::string &, Type *);
};

class SymbolTableFunction: public SymbolTable {
  public:
    SymbolTableFunction(SymbolTable *parent);

    Symbol *addVar(const std::string &, Type *);
    Symbol *addType(const std::string &, Type *);

    void setStackOffsets();
    int32_t getStackSize() { return stackOffset; }

  protected:
    int32_t stackOffset;
    SymbolTable *parent;
};

class SymbolTableClosure: public SymbolTableFunction {
  public:
    SymbolTableClosure(SymbolTable *parent);

    Symbol *resolve(const std::string &);

    void setHeapOffsets();
    int32_t getHeapVariableSize() { return heapOffset; }

  private:
    int32_t heapOffset;
};

#endif
