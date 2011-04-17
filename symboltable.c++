#include "symboltable.h"

#include "library.h"
#include "types.h"

#include <stdexcept>
#include <cassert>
#include <iostream>

int32_t SymbolLocalVariable::INVALID = std::numeric_limits<int32_t>::min();
int32_t SymbolClosureVariable::INVALID = std::numeric_limits<int32_t>::min();

Symbol *SymbolTable::resolve(const std::string &id) {
  auto hit = symbols.find(id);
  if(hit != symbols.end()) return hit->second;

  for(auto i = searchPath.begin(); i != searchPath.end(); ++i) {
    Symbol *hit = (*i)->resolve(id);
    if(hit) return hit;
  }

  return 0;
}

Symbol *SymbolTable::addSymbol(const std::string &id, Symbol *sym) {
  if(symbols.find(id) != symbols.end()) {
    throw std::runtime_error("duplicate symbol definition " + id);
  }

  symbols[id] = sym;
  return sym;
}

Symbol *SymbolTableGlobal::addVar(const std::string &name, Type *type) {
  SymbolGlobalVariable *var =
    (new SymbolGlobalVariable())
    ->setType(type)
    ->setAddress(Library::newObject(type->getSize()));

  return addSymbol(name, var);
}

Symbol *SymbolTableGlobal::addType(const std::string &name, Type *type) {
  SymbolType *symbol =
    (new SymbolType())
    ->setType(type);

  return addSymbol(name, symbol);
}

SymbolTableFunction::SymbolTableFunction(SymbolTable *parent)
  : stackOffset(0), parent(parent) {
  searchPath.push_back(parent);
}

Symbol *SymbolTableFunction::addVar(const std::string &id, Type *type) {
  SymbolLocalVariable *var =
    (new SymbolLocalVariable())
    ->setType(type);

  return SymbolTable::addSymbol(id, var);
}

Symbol *SymbolTableFunction::addType(const std::string &name, Type *type) {
  SymbolType *symbol =
    (new SymbolType())
    ->setType(type);

  return addSymbol(name, symbol);
}

void SymbolTableFunction::setStackOffsets() {
  if(SymbolTableFunction *fparent = dynamic_cast<SymbolTableFunction *>(parent)) {
    stackOffset = fparent->stackOffset;
  }

  for(auto i = symbols.begin(); i != symbols.end(); ++i) {
    if(SymbolLocalVariable *var = dynamic_cast<SymbolLocalVariable *>(i->second)) {
      stackOffset -= var->getType()->getSize();
      var->setStackOffset(stackOffset);
    }
  }
}

SymbolTableClosure::SymbolTableClosure(SymbolTable *parent)
  : SymbolTableFunction(parent), heapOffset(0) { }

Symbol *SymbolTableClosure::resolve(const std::string &id) {
  auto hit = symbols.find(id);
  if(hit != symbols.end()) return hit->second;

  Symbol *parent = SymbolTableFunction::resolve(id);
  if(!parent) {
    throw std::runtime_error("cannot resolve symbol '" + id + "' while closuring");
  }
  
  if(SymbolVariable *var = dynamic_cast<SymbolVariable *>(parent)) {
    return symbols[id] = (new SymbolClosureVariable())
      ->setType(var->getType())
      ->setParentSymbol(var);
  }

  return parent;
}

void SymbolTableClosure::setHeapOffsets() {
  assert(!heapOffset);

  for(auto i = symbols.begin(); i != symbols.end(); ++i) {
    // this number is the offset of the data fields from after the
    // call instruction preparing r8 + the jump target for said call instruction
    if(!heapOffset) heapOffset = 16;

    if(SymbolClosureVariable *var = dynamic_cast<SymbolClosureVariable *>(i->second)) {
      var->setHeapOffset(heapOffset);
      heapOffset += var->getType()->getSize();
    }
  }
}
