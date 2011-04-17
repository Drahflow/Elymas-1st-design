#include "treenodes.h"

#include "opcodes.h"
#include "assembly.h"
#include "types.h"
#include "symboltable.h"
#include "library.h"

#include <iostream>
#include <stdexcept>
#include <sstream>

using namespace opcode;

void NodeStatementExpr::rewriteDeclarations(SymbolTable *st) {
  expr->rewriteDeclarations(st, &expr);
}

void NodeStatementExpr::resolveSymbols(SymbolTable *st) {
  expr->resolveSymbols(st);
}

void NodeStatementExpr::rewriteFunctionApplications() {
  expr->rewriteFunctionApplications(&expr);
}

bool NodeStatementExpr::assignUnresolvedTypes(Type *t) {
  return expr->assignUnresolvedTypes(t);
}

void NodeStatementExpr::compile(Assembly &assembly) {
  expr->compile(assembly);
}

std::string NodeStatementExpr::dump(int i) {
  return indent(i) + "StatementExpr\n" + expr->dump(i + 2);
}

void NodeStatementWhile::rewriteDeclarations(SymbolTable *st) {
  condition->rewriteDeclarations(st, &condition);
  body->rewriteDeclarations(st);
}

void NodeStatementWhile::resolveSymbols(SymbolTable *st) {
  condition->resolveSymbols(st);
  body->resolveSymbols(st);
}

void NodeStatementWhile::rewriteFunctionApplications() {
  condition->rewriteFunctionApplications(&condition);
  body->rewriteFunctionApplications();
}

bool NodeStatementWhile::assignUnresolvedTypes(Type *t) {
  condition->assignUnresolvedTypes(Type::boolean);
  return body->assignUnresolvedTypes(t);
}

void NodeStatementWhile::compile(Assembly &) {
  assert(false);
}

void NodeStatementGoto::resolveSymbols(SymbolTable *st) {
  target->resolveSymbols(st);
}

void NodeStatementGoto::rewriteFunctionApplications() {
  target->rewriteFunctionApplications(&target);
}

bool NodeStatementGoto::assignUnresolvedTypes(Type *t) {
  return target->assignUnresolvedTypes(t);
}

void NodeStatementGoto::compile(Assembly &) {
  assert(false);
}

void NodeStatementLabel::resolveSymbols(SymbolTable *st) {
  name->resolveSymbols(st);
}

void NodeStatementLabel::rewriteFunctionApplications() {
  // no rewriting the identifier so far
}

bool NodeStatementLabel::assignUnresolvedTypes(Type *t) {
  return name->assignUnresolvedTypes(t);
}

void NodeStatementLabel::compile(Assembly &) {
  assert(false);
}

void NodeStatementRule::rewriteDeclarations(SymbolTable *st) {
  mapping->rewriteDeclarations(st);
}

void NodeStatementRule::resolveSymbols(SymbolTable *st) {
  lhs->resolveSymbols(st);
  rhs->resolveSymbols(st);
  mapping->resolveSymbols(st);
}

void NodeStatementRule::rewriteFunctionApplications() {
  mapping->rewriteFunctionApplications();
}

bool NodeStatementRule::assignUnresolvedTypes(Type *) {
  assert(false);
}

void NodeStatementRule::compile(Assembly &) {
  assert(false);
}

void NodeStatementBlock::rewriteDeclarations(SymbolTable *st) {
  // TODO: open new SymbolTable already...

  for(auto i = statements.begin(); i != statements.end(); ++i) {
    (*i)->rewriteDeclarations(st);
  }
}

void NodeStatementBlock::resolveSymbols(SymbolTable *st) {
  for(auto i = statements.begin(); i != statements.end(); ++i) {
    (*i)->resolveSymbols(st);
  }
}

void NodeStatementBlock::rewriteFunctionApplications() {
  for(auto i = statements.begin(); i != statements.end(); ++i) {
    (*i)->rewriteFunctionApplications();
  }
}

bool NodeStatementBlock::assignUnresolvedTypes(Type *t) {
  for(auto i = statements.begin(); i != statements.end(); ++i) {
    if(!(*i)->assignUnresolvedTypes(t)) return false;
  }

  return true;
}

void NodeStatementBlock::compile(Assembly &assembly) {
  for(auto i = statements.begin(); i != statements.end(); ++i) {
    (*i)->compile(assembly);
  }
}

std::string NodeExprBinary::dump(int i) {
  return indent(i) + op() + "\n"
    + lhs->dump(i + 2) + "\n"
    + rhs->dump(i + 2);
}

void NodeExprUnary::rewriteDeclarations(SymbolTable *st, NodeExpr **) {
  arg->rewriteDeclarations(st, &arg);
}

void NodeExprUnary::resolveSymbols(SymbolTable *st) {
  arg->resolveSymbols(st);
}

void NodeExprUnary::rewriteFunctionApplications(NodeExpr **) {
  arg->rewriteFunctionApplications(&arg);
}

bool NodeExprUnary::assignUnresolvedTypes(Type *t) {
  return arg->assignUnresolvedTypes(t);
}

void NodeExprUnary::compile(Assembly &) {
  assert(false);
}

void NodeExprUnary::compileL(Assembly &) {
  assert(false);
}

Type *NodeExprUnary::getType() {
  return arg->getType();
}

std::string NodeExprUnary::dump(int i) {
  return indent(i) + op() + "\n" + arg->dump(i + 2);
}

bool NodeIdentifier::assignUnresolvedTypes(Type *t) {
  return getType()->canConvertTo(t);
}

void NodeIdentifier::rewriteDeclarations(SymbolTable *st, NodeExpr **parent) {
  symbol = st->resolve(id);
  if(!symbol) return;

  if(SymbolType *type = dynamic_cast<SymbolType *>(symbol)) {
    *parent = type->getType();
  }
}

void NodeIdentifier::resolveSymbols(SymbolTable *st) {
  symbol = st->resolve(id);
  if(!symbol) {
    compileError("cannot resolve " + id);
  }
}

std::string NodeIdentifier::dump(int i) {
  return indent(i) + id;
}

static void compileRValue(Assembly &assembly, Symbol *symbol) {
  if(SymbolGlobalVariable *variable =
      dynamic_cast<SymbolGlobalVariable *>(symbol)) {
    assembly.add(move(
        reinterpret_cast<uint64_t>(variable->getAddress()),
        rax()));
    assembly.add(move(memory(rax()), rax()));
  } else if(SymbolLocalVariable *variable =
      dynamic_cast<SymbolLocalVariable *>(symbol)) {
    assembly.add(move(memory(variable->getStackOffset(), rbp()), rax()));
  } else if(SymbolClosureVariable *variable =
      dynamic_cast<SymbolClosureVariable *>(symbol)) {
    assembly.add(move(memory(variable->getHeapOffset(), r8()), rax()));
  } else {
    throw std::runtime_error("didn't know how to compile rvalue for symbol");
  }
}

void NodeIdentifier::compile(Assembly &assembly) {
  compileRValue(assembly, symbol);
}

static void compileLValue(Assembly &assembly, Symbol *symbol) {
  if(SymbolGlobalVariable *variable =
      dynamic_cast<SymbolGlobalVariable *>(symbol)) {
    assembly.add(move(
        reinterpret_cast<uint64_t>(variable->getAddress()),
        rax()));
  } else if(SymbolLocalVariable *variable =
      dynamic_cast<SymbolLocalVariable *>(symbol)) {
    assembly.add(lea(memory(variable->getStackOffset(), rbp()), rax()));
  } else if(SymbolClosureVariable *variable =
      dynamic_cast<SymbolClosureVariable *>(symbol)) {
    assembly.add(lea(memory(variable->getHeapOffset(), r8()), rax()));
  } else {
    throw std::runtime_error("didn't know how to compile rvalue for symbol");
  }
}

void NodeIdentifier::compileL(Assembly &assembly) {
  compileLValue(assembly, symbol);
}

Type *NodeIdentifier::getType() {
  SymbolVariable *variable = dynamic_cast<SymbolVariable *>(symbol);
  assert(variable);

  return variable->getType();
}

void NodeInteger::resolveSymbols(SymbolTable *) {
  // nothing to do
}

bool NodeInteger::assignUnresolvedTypes(Type *t) {
  return Type::sint32->canConvertTo(t);
}

void NodeInteger::compile(Assembly &assembly) {
  assembly.add(move(val, rax()));
}

void NodeInteger::compileL(Assembly &) {
  assert(false);
}

Type *NodeInteger::getType() {
  // TODO: use value to determine type
  return Type::sint32;
}

void NodeString::resolveSymbols(SymbolTable *) {
  // nothing to do
}

bool NodeString::assignUnresolvedTypes(Type *) {
  assert(false);
}

void NodeString::compile(Assembly &) {
  assert(false);
}

void NodeString::compileL(Assembly &) {
  assert(false);
}

Type *NodeString::getType() {
  assert(false);
}

NodeExprTuple::NodeExprTuple(NodeExprList *list): type(0) {
  assert(list);

  for(auto i = list->exprs.begin(); i != list->exprs.end(); ++i) {
    if(NodeExprTuple *tuple = dynamic_cast<NodeExprTuple *>(*i)) {
      std::copy(tuple->elements.begin(), tuple->elements.end(),
          back_inserter(elements));
    } else {
      elements.push_back(*i);
    }
  }
}

NodeExprTuple::NodeExprTuple(NodeExpr *expr1): type(0) {
  assert(expr1);

  if(NodeExprTuple *tuple = dynamic_cast<NodeExprTuple *>(expr1)) {
    std::copy(tuple->elements.begin(), tuple->elements.end(),
        back_inserter(elements));
  } else {
    elements.push_back(expr1);
  }
}

NodeExprTuple::NodeExprTuple(NodeExpr *expr1, NodeExpr *expr2): type(0) {
  assert(expr1);
  assert(expr2);

  if(NodeExprTuple *tuple = dynamic_cast<NodeExprTuple *>(expr1)) {
    std::copy(tuple->elements.begin(), tuple->elements.end(),
        back_inserter(elements));
  } else {
    elements.push_back(expr1);
  }

  if(NodeExprTuple *tuple = dynamic_cast<NodeExprTuple *>(expr2)) {
    std::copy(tuple->elements.begin(), tuple->elements.end(),
        back_inserter(elements));
  } else {
    elements.push_back(expr2);
  }
}

void NodeExprTuple::rewriteDeclarations(SymbolTable *st, NodeExpr **parent) {
  for(auto i = elements.begin(); i != elements.end(); ++i) {
    (*i)->rewriteDeclarations(st, &*i);
  }

  if(elements.size() && dynamic_cast<Type *>(elements.front())) {
    if(elements.size() == 1) {
      Type *type = dynamic_cast<Type *>(elements.front());
      assert(type);

      assert(parent);
      *parent = type;
    } else {
      TypeTuple *typeTuple = new TypeTuple();

      for(auto i = elements.begin(); i != elements.end(); ++i) {
        Type *type = dynamic_cast<Type *>(*i);
        assert(type);

        typeTuple->addElementType(type);
      }

      assert(parent);
      *parent = typeTuple;
    }
  }
}

void NodeExprTuple::resolveSymbols(SymbolTable *st) {
  for(auto i = elements.begin(); i != elements.end(); ++i) {
    (*i)->resolveSymbols(st);
  }
}

void NodeExprTuple::rewriteFunctionApplications(NodeExpr **parent) {
  if(elements.size() == 0) {
    assert(false);
  } else if(elements.size() == 1) {
    *parent = elements.front();
    (*parent)->rewriteFunctionApplications(parent);
  } else {
    for(auto i = elements.begin(); i != elements.end(); ++i) {
      (*i)->rewriteFunctionApplications(&*i);
    }
  }
}

bool NodeExprTuple::assignUnresolvedTypes(Type *t) {
  if(elements.size() == 1) {
    return elements[0]->assignUnresolvedTypes(t);
  } else if(auto tt = dynamic_cast<TypeTuple *>(t)) {
    for(size_t i = 0; i < tt->getTupleWidth(); ++i) {
      if(!elements[i]->assignUnresolvedTypes(tt->getElementType(i))) return false;
    }

    return true;
  } else {
    for(auto i = elements.begin(); i != elements.end(); ++i) {
      (*i)->assignUnresolvedTypes(Type::any);
    }

    return getType()->canConvertTo(t);
  }
}

template<class T> static void allocImpl(Assembly &assembly, T size) {
  assembly.add(push(rcx()));
  assembly.add(push(rdx()));
  assembly.add(push(rsi()));
  assembly.add(push(rdi()));
  assembly.add(push(r8()));
  assembly.add(push(r9()));
  assembly.add(push(r10()));
  assembly.add(push(r11()));
  assembly.add(cld());
  assembly.add(move(size, rdi()));
  assembly.add(move(reinterpret_cast<uint64_t>(&Library::newObject), rax()));
  assembly.add(call(rax())); // no immediate 64bit calls available
  assembly.add(pop(r11()));
  assembly.add(pop(r10()));
  assembly.add(pop(r9()));
  assembly.add(pop(r8()));
  assembly.add(pop(rdi()));
  assembly.add(pop(rsi()));
  assembly.add(pop(rdx()));
  assembly.add(pop(rcx()));
}

template<class T> static void alloc(Assembly &assembly, T size) { allocImpl(assembly, size); }
template<> void alloc<unsigned int>(Assembly &assembly, unsigned int size) {
  assert(!(size & 3));

  allocImpl(assembly, size);
}

void NodeExprTuple::compile(Assembly &assembly) {
  getType();
  assert(type);

  if(elements.size() != 1) {
    auto typet = dynamic_cast<TypeTuple *>(type);
    assert(typet);

    alloc(assembly, typet->getHeapSize());
    assembly.add(push(rcx()));
    assembly.add(move(rax(), rcx()));

    int32_t offset = 0;
    for(size_t i = 0; i < elements.size(); ++i) {
      elements[i]->compile(assembly);

      switch(elements[i]->getType()->getSize()) {
        case 8:
          assembly.add(move(rax(), memory(offset, rcx())));
          break;
        default:
          // TODO: handle other sizes
          assert(false);
      }

      offset += elements[i]->getType()->getSize();
    }

    assembly.add(move(rcx(), rax()));
    assembly.add(pop(rcx()));
  } else {
    elements[0]->compile(assembly);
  }
}

void NodeExprTuple::compileL(Assembly &) {
  assert(false);
}

Type *NodeExprTuple::getType() {
  if(elements.size() == 1) {
    type = elements[0]->getType();
  } else {
    TypeTuple *typet = new TypeTuple();
    for(auto i = elements.begin(); i != elements.end(); ++i) {
      Type *et = (*i)->getType();
      if(auto ett = dynamic_cast<TypeTuple *>(et)) {
        for(size_t j = 0; j < ett->getTupleWidth(); ++j) {
          typet->addElementType(ett->getElementType(j));
        }
      } else {
        typet->addElementType(et);
      }
    }

    type = typet;
  }

  return type;
}

std::string NodeExprTuple::dump(int i) {
  std::ostringstream str;
  str << indent(i) << "NodeExprTuple";
  for(auto j = elements.begin(); j != elements.end(); ++j) {
    str << "\n" + (*j)->dump(i + 2);
  }
  return str.str();
}

class TypeArray: public TypeLoopable {
  public:
    TypeArray(Type *inner): TypeLoopable(inner) { }

    bool canConvertTo(Type *other) {
      // TODO: maybe allow a bit more one day
      return *this == *other;
    }

    bool operator == (Type &other) {
      TypeArray *array = dynamic_cast<TypeArray *>(&other);
      if(!array) return false;

      return *innerType == *array->innerType;
    }

    unsigned int getSize() { return 8; }

    Type *generate(Type *inner) {
      return new TypeArray(inner);
    }

    // TODO: handle co-looping correctly

    struct LoopLabels {
      Label *start;
      Label *end;
    };

    void loopGenerate(Assembly &assembly, Type *result) {
      assembly.add(push(rdx()));
      assembly.add(push(rdi()));

      assembly.add(move(memory(-8, rax()), rax()));
      // TODO: mask lower bits
      assembly.add(move(innerType->getSize(), rdi()));
      assembly.add(move(static_cast<uint64_t>(0), rdx()));
      assembly.add(div(rdi()));
      assembly.add(move(result->getSize(), rdi()));
      assembly.add(mul(rdi()));
      alloc(assembly, rax());

      assembly.add(pop(rdi()));
      assembly.add(pop(rdx()));
    }

    void *loopBegin(Assembly &assembly, Type *) {
      LoopLabels *labels = new LoopLabels();
      labels->start = assembly.label();
      labels->end = assembly.label();

      assembly.add(move(static_cast<uint64_t>(0), rcx()));
      assembly.add(labels->start);
      assembly.add(move(memory(-8, rax()), rax()));
      // TODO: mask lower 3 bits
      switch(innerType->getSize()) {
        case 8: assembly.add(shr(3, rax())); break;
        default: assert(false);
      }
      assembly.add(cmp(rax(), rcx()));
      assembly.add(jae(labels->end));

      return labels;
    }

    void loopStep(Assembly &assembly, Type *) {
      assembly.add(add(static_cast<int32_t>(1), rcx()));
    }

    void loopAdjustKey(Assembly &, Type *) {
      assert(false);
    }

    void loopSaveResult(Assembly &assembly, Type *result) {
      switch(result->getSize()) {
        case 8: assembly.add(move(rax(), memory(rdx(), rcx(), 8))); break;
        default:
          assert(false);
      }
    }

    void loopEnd(Assembly &assembly, Type *, void *data) {
      LoopLabels *labels = reinterpret_cast<LoopLabels *>(data);
      assembly.add(jmp(labels->start));
      assembly.add(labels->end);

      delete labels;
    }

    void compile(Assembly &) {
      // TODO: initialize arrays
      assert(false);
    }

    int getArgumentRank(unsigned int) {
      return 1;
    }

    Type *getArgumentType(unsigned int) {
      return Type::sint32;
    }

    void dereference(Assembly &assembly) {
      switch(innerType->getSize()) {
        case 8: assembly.add(move(memory(rax(), rcx(), 8), rax()));
                break;
        default: assert(false);
      }
    }

    bool isConcrete() {
      return innerType->isConcrete();
    }
};

void NodeExprArray::rewriteDeclarations(SymbolTable *st, NodeExpr **parent) {
  for(auto i = elements.begin(); i != elements.end(); ++i) {
    (*i)->rewriteDeclarations(st, &*i);
  }

  if(elements.size() && dynamic_cast<Type *>(elements.front())) {
    if(elements.size() != 1) compileError("array types are declared with a single type element");

    *parent = new TypeArray(dynamic_cast<Type *>(elements.front()));
  }
}

void NodeExprArray::resolveSymbols(SymbolTable *st) {
  for(auto i = elements.begin(); i != elements.end(); ++i) {
    (*i)->resolveSymbols(st);
  }
}

void NodeExprArray::rewriteFunctionApplications(NodeExpr **) {
  for(auto i = elements.begin(); i != elements.end(); ++i) {
    (*i)->rewriteFunctionApplications(&*i);
  }
}

bool NodeExprArray::assignUnresolvedTypes(Type *t) {
  Type *elementType = Type::any;

  if(auto at = dynamic_cast<TypeLoopable *>(t)) {
    elementType = at->getReturnType();
  }

  for(auto i = elements.begin(); i != elements.end(); ++i) {
    if(!(*i)->assignUnresolvedTypes(elementType)) return false;
  }

  return true;
}

void NodeExprArray::compile(Assembly &assembly) {
  uint64_t size = 0;
  uint64_t elemSize = 0;

  if(elements.size()) {
    elemSize = elements[0]->getType()->getSize();
    size = elemSize * elements.size();
  }

  alloc(assembly, size);
  assembly.add(push(rcx()));
  assembly.add(move(rax(), rcx()));

  for(size_t i = 0; i < elements.size(); ++i) {
    elements[i]->compile(assembly);

    switch(elemSize) {
      case 8:
        assembly.add(move(rax(), memory(i * elemSize, rcx())));
        break;
      default:
        // TODO: handle other sizes
        assert(false);
    }
  }

  assembly.add(move(rcx(), rax()));
  assembly.add(pop(rcx()));
}

void NodeExprArray::compileL(Assembly &) {
  assert(false);
}

Type *NodeExprArray::getType() {
  Type *inner = Type::any;

  if(elements.size()) {
    inner = elements[0]->getType();

    for(size_t i = 1; i < elements.size(); ++i) {
      if(*elements[i]->getType() != *inner) {
        compileError("array of non-equal types " + this->dump(0));
      }
    }
  }
  
  return new TypeArray(inner);
}

std::string NodeExprArray::dump(int i) {
  std::string ret = indent(i) + "NodeExprArray";
  for(auto j = elements.begin(); j != elements.end(); ++j) {
    ret += "\n" + (*j)->dump(i + 2);
  }
  return ret;
}

NodeExprApply::NodeExprApply(NodeExpr *function, NodeExpr *argument)
  : function(function), argument(argument), syms(0), type(Type::any) {
  assert(function);
  assert(argument);
}

void NodeExprApply::rewriteDeclarations(SymbolTable *st, NodeExpr **parent) {
  function->rewriteDeclarations(st, &function);
  if(Type *type = dynamic_cast<Type *>(function)) {
    NodeIdentifier *name = dynamic_cast<NodeIdentifier *>(argument);
    if(!name) {
      compileError("declaration of something not a simple identifier");
    }

    *parent = new NodeExprDeclaration(type, name);
    (*parent)->rewriteDeclarations(st, parent);
  } else {
    argument->rewriteDeclarations(st, &argument);
  }
}

void NodeExprApply::resolveSymbols(SymbolTable *st) {
  function->resolveSymbols(st);
  argument->resolveSymbols(st);

  syms = st;
}

static std::string createUniqueIdentifier() {
  static unsigned long long id = 0;

  std::ostringstream str;
  str << "#" << id++;
  return str.str();
}

void NodeExprApply::abstractTypeDomainedFull(NodeExpr **parent, TypeDomained *td) {
  //std::cout << "Apply::abstractFull" << std::endl;

  auto argName = createUniqueIdentifier();
  auto keyName = createUniqueIdentifier();

  if(dynamic_cast<TypeLoopable *>(td)) {
    *parent = new NodeExprLoop(argument, argName,
        new NodeIdentifier(argName), keyName,
        new NodeExprApply(function, new NodeExprApply(new NodeIdentifier(argName), new NodeIdentifier(keyName))));
  } else if(auto tdf = dynamic_cast<TypeFunction *>(td)) {
    NodeExprList *replacementArguments = new NodeExprList();
    NodeExprList *replacementCall = new NodeExprList();

    for(size_t i = 0; i < tdf->getArgumentCount(); ++i) {
      auto id = createUniqueIdentifier();

      assert(tdf->getArgumentType(i)->isConcrete());
      replacementArguments->add(new NodeExprDeclaration(tdf->getArgumentType(i), new NodeIdentifier(id)));
      replacementCall->add(new NodeIdentifier(id));
    }

    auto ft = dynamic_cast<TypeFunction *>(function->getType());
    assert(ft);

    *parent = new NodeTypedLambda(new NodeExprTuple(replacementArguments), ft->getReturnType(),
        new NodeStatementExpr(new NodeExprApply(function, new NodeExprApply(argument, new NodeExprTuple(replacementCall)))));
  }

  assert(syms);
  (*parent)->rewriteDeclarations(syms, parent);
  (*parent)->resolveSymbols(syms);
  (*parent)->assignUnresolvedTypes(getType());
  (*parent)->rewriteFunctionApplications(parent);
}

void NodeExprApply::abstractTypeDomainedPositioned(NodeExpr **parent, const std::vector<int> &positions) {
  //std::cout << "Apply::abstractPositioned" << std::endl;
  //for(auto i = positions.begin(); i != positions.end(); ++i) {
  //  std::cout << *i << std::endl;
  //}

  auto *ft = dynamic_cast<TypeFunction *>(function->getType());
  assert(ft);

  int primary = positions.front();

  Type *at = argument->getType();
  if(auto att = dynamic_cast<TypeTuple *>(at)) {
    at = att->getElementType(primary);
  } else {
    assert(primary == 0);
  }

  auto primaryType = dynamic_cast<TypeDomained *>(at);
  assert(primaryType);

  if(auto prf = dynamic_cast<TypeFunction *>(primaryType)) {
    NodeExprList *replacementArguments = new NodeExprList();
    NodeExprList *replacementCall = new NodeExprList();

    std::vector<std::string> argNames;
    for(size_t i = 0; i < prf->getArgumentCount(); ++i) {
      auto id = createUniqueIdentifier();
      argNames.push_back(id);

      assert(prf->getArgumentType(i)->isConcrete());
      replacementArguments->add(new NodeExprDeclaration(prf->getArgumentType(i), new NodeIdentifier(id)));
    }

    for(size_t i = 0; i < ft->getArgumentCount(); ++i) {
      if(std::find(positions.begin(), positions.end(), i) == positions.end()) {
        assert(i < argNames.size());
        replacementCall->add(new NodeIdentifier(argNames[i]));
      } else {
        NodeExpr *fun = argument;
        if(auto *funt = dynamic_cast<NodeExprTuple *>(fun)) {
          fun = funt->getElements()[i];
        }

        // TODO: think about more magical domain mappings
        NodeExprList *innerArgs = new NodeExprList();
        for(auto j = argNames.begin(); j != argNames.end(); ++j) {
          innerArgs->add(new NodeIdentifier(*j));
        }

        replacementCall->add(new NodeExprApply(fun, new NodeExprTuple(innerArgs)));
      }
    }

    *parent = new NodeTypedLambda(new NodeExprTuple(replacementArguments), ft->getReturnType(),
        new NodeStatementExpr(new NodeExprApply(function, new NodeExprTuple(replacementCall))));
  } else if(auto ptl = dynamic_cast<TypeLoopable *>(primaryType)) {
    auto argName = createUniqueIdentifier();
    auto keyName = createUniqueIdentifier();

    NodeExprList *replacementCall = new NodeExprList();

    if(dynamic_cast<TypeTuple *>(argument->getType())) {
      for(size_t i = 0; i < ft->getArgumentCount(); ++i) {
        if(std::find(positions.begin(), positions.end(), i) == positions.end()) {
          TypeFunction *pt = (new TypeFunction(*ft))->setReturnType(ft->getArgumentType(i));
          replacementCall->add(new NodeExprApply(new NodeExprProjection(i, 0, pt), new NodeIdentifier(argName)));
        } else {
          TypeFunction *pt = (new TypeFunction(*ft))->setReturnType(ptl->generate(ft->getArgumentType(i)));
          replacementCall->add(new NodeExprApply(
                new NodeExprApply(new NodeExprProjection(i, 0, pt), new NodeIdentifier(argName)),
                new NodeIdentifier(keyName)));
        }
      }
    } else {
      assert(ft->getArgumentCount() == 1);

      replacementCall->add(new NodeExprApply(
            new NodeIdentifier(argName),
            new NodeIdentifier(keyName)));
    }

    NodeExpr *container;
    if(auto att = dynamic_cast<TypeTuple *>(argument->getType())) {
      if(att->getTupleWidth() > 1) {
        TypeFunction *pt = (new TypeFunction(*ft))->setReturnType(ptl->generate(ft->getArgumentType(primary)));
        container = new NodeExprApply(new NodeExprProjection(primary, 0, pt), argument);
      }
    } else {
        container = new NodeIdentifier(argName);
    }

    *parent = new NodeExprLoop(argument, argName,
        container, keyName,
        new NodeExprApply(function, new NodeExprTuple(replacementCall)));
  } else {
    compileError("overload resolution fucked up, "
        "autolooping is deeply sorry (4): " + function->dump(0));
  }

  //std::cout << "vvv rewriting finished vvv" << std::endl;
  //std::cout << (*parent)->dump(0) << std::endl;
  //std::cout << "^^^ rewriting finished ^^^" << std::endl;

  assert(syms);
  (*parent)->rewriteDeclarations(syms, parent);
  (*parent)->resolveSymbols(syms);
  (*parent)->assignUnresolvedTypes(getType());
  (*parent)->rewriteFunctionApplications(parent);
}

static bool needsAbstraction(Type *wanted, Type *given, int rank) {
  assert(rank == -1 || rank > 0);
  bool needs;

  if(rank == -1) {
    needs = !given->canConvertTo(wanted);
  } else if(!given->canConvertTo(wanted)) {
    needs = true;
  } else {
    int count;

    for(count = 0; count <= rank; ) {
      count += given->canConvertTo(wanted);

      auto givenTd = dynamic_cast<TypeDomained *>(given);
      if(!givenTd) break;

      given = givenTd->getReturnType();
    }
    
    assert(count >= rank); // sufficient type depth

    needs = count > rank;
  }
  
  if(needs) {
    assert(dynamic_cast<TypeDomained *>(given));
  }

  return needs;
}

// 1. from top to bottom search for tuples of right count
//    one-tuples and scalars are skipped (unless function has 1 argument)
//    lists are looped (but if 1 argument function, top level tuple matches)
// 2. for each argument of the function, search within the
//    appropriate element of the tuple, with the search algorithm
//    specified by rank of the argument
//    one-tuples and scalars are skipped (unless matching)
//    lists are looped (if traversed)
// 3. create unified (except for initial tuple scan) loops
//    start from top of types
//    3a. take the type of the leftmost argument
//    3b. all arguments with a compatible loopable type are looped
//        the result dimension is of the looped type (or their supertypes)
//    3c. all arguments with an incomptible type are not looped but
//        replicated along the dimension
//    3d. all arguments looped have their topmost type removed accordingly
// 4. (repeatedly) invoke the function until all elements have
//    been processed
// TODO: combine the code with the assignUnresolvedTypes code below
void NodeExprApply::rewriteFunctionApplications(NodeExpr **parent) {
  function->rewriteFunctionApplications(&function);
  argument->rewriteFunctionApplications(&argument);

  if(auto tt = dynamic_cast<TypeTuple *>(function->getType())) {
    assert(tt->getTupleWidth() != 1);
    assert(false); // TODO: this should produce a tuple of the results
  }

  TypeDomained *ft = dynamic_cast<TypeDomained *>(function->getType());
  if(!ft) compileError("cannot apply non-function " + function->dump(0));

  Type *at = argument->getType();

  if(ft->getArgumentCount() != 1) {
    while(1) {
      if(auto *att = dynamic_cast<TypeTuple *>(at)) {
        if(att->getTupleWidth() != 1) break;
        at = att->getElementType(0);
      } else if(auto *atd = dynamic_cast<TypeDomained *>(at)) {
        abstractTypeDomainedFull(parent, atd);
        return;
      } else {
        compileError("overload resolution fucked up, "
            "autolooping is deeply sorry (1): " + function->dump(0));
      }
    }
  }

  if(auto att = dynamic_cast<TypeTuple *>(at)) {
    if(att->getTupleWidth() < ft->getArgumentCount()) {
      compileError("overload resolution fucked up, "
          "autolooping is deeply sorry (2a): " + function->dump(0));
    }
  } else if(ft->getArgumentCount() > 1) {
    compileError("overload resolution fucked up, "
        "autolooping is deeply sorry (2b): " + function->dump(0));
  }

  std::vector<int> abstractionPositions;
  TypeDomained *abstractionPrimary = 0;

  for(size_t i = 0; i < ft->getArgumentCount(); ++i) {
    Type *fargt = ft->getArgumentType(i);
    Type *argt = at;
    if(auto *argtt = dynamic_cast<TypeTuple *>(argt)) {
      argt = argtt->getElementType(i);
    }

    int rank = ft->getArgumentRank(i);
    assert(rank);

    if(!needsAbstraction(fargt, argt, rank)) continue;

    if(!abstractionPrimary) {
      abstractionPositions.push_back(i);
      abstractionPrimary = dynamic_cast<TypeDomained *>(argt);
      assert(abstractionPrimary);
    } else if(dynamic_cast<TypeDomained *>(argt) && abstractionPrimary->canTakeDomainFrom(dynamic_cast<TypeDomained *>(argt))) {
      int tmp = abstractionPositions.front();
      abstractionPositions.push_back(tmp);
      abstractionPositions.front() = i;

      abstractionPrimary = dynamic_cast<TypeDomained *>(argt);
      assert(abstractionPrimary);
    } else if(dynamic_cast<TypeDomained *>(argt) && dynamic_cast<TypeDomained *>(argt)->canTakeDomainFrom(abstractionPrimary)) {
      abstractionPositions.push_back(i);
    }
  }

  if(!abstractionPositions.empty()) {
    abstractTypeDomainedPositioned(parent, abstractionPositions);
    return;
  }
}

// TODO: maybe some linear algeabra might help with figuring the right types out
bool NodeExprApply::assignUnresolvedTypes(Type *t) {
  //std::cout << dump(0) << std::endl;
  //std::cout << "analysing types..." << std::endl;

  function->assignUnresolvedTypes(Type::any);
  argument->assignUnresolvedTypes(Type::any);

  Type *oldFt;
  Type *oldAt;

  for(int i = 0; i < 16; ++i) {
    oldFt = function->getType();
    oldAt = argument->getType();

    TypeFunction *ft = new TypeFunction();

    if(auto att = dynamic_cast<TypeTuple *>(oldAt)) {
      for(size_t i = 0; i < att->getTupleWidth(); ++i) {
        ft->addArgument(att->getElementType(i), 1);
      }
    } else {
      ft->addArgument(oldAt, 1);
    }
    ft->setReturnType(Type::any);

    function->assignUnresolvedTypes(ft);

    TypeDomained *newFt = dynamic_cast<TypeDomained *>(function->getType());
    assert(newFt);

    if(newFt->getArgumentCount() == 1) {
      argument->assignUnresolvedTypes(newFt->getArgumentType(0));
    } else {
      TypeTuple *newAt = new TypeTuple();

      for(size_t i = 0; i < newFt->getArgumentCount(); ++i) {
        newAt->addElementType(newFt->getArgumentType(i));
      }

      argument->assignUnresolvedTypes(newAt);
    }

    if(*oldFt == *function->getType() && *oldAt == *argument->getType()) break;
  }

  auto ft = dynamic_cast<TypeDomained *>(function->getType());
  assert(ft);

  if(auto tt = dynamic_cast<TypeTuple *>(function->getType())) {
    assert(tt->getTupleWidth() != 1);
    assert(false); // TODO: this should produce a tuple of the results
  }

  Type *at = argument->getType();
  std::vector<TypeDomained *> unloopedDomains;

  if(ft->getArgumentCount() != 1) {
    while(1) {
      if(auto *att = dynamic_cast<TypeTuple *>(at)) {
        if(att->getTupleWidth() != 1) break;
        at = att->getElementType(0);
      } else if(auto *atd = dynamic_cast<TypeDomained *>(at)) {
        at = atd->getReturnType();
        unloopedDomains.push_back(atd);
      } else {
        compileError("overload resolution fucked up, "
            "autolooping is deeply sorry (t1): " + function->dump(0));
      }
    }
  }

  if(auto att = dynamic_cast<TypeTuple *>(at)) {
    if(att->getTupleWidth() < ft->getArgumentCount()) {
      compileError("overload resolution fucked up, "
          "autolooping is deeply sorry (t2a): " + function->dump(0));
    }
  } else if(ft->getArgumentCount() > 1) {
    compileError("overload resolution fucked up, "
        "autolooping is deeply sorry (t2b): " + function->dump(0));
  }

  std::vector<Type *> argts;
  for(size_t i = 0; i < ft->getArgumentCount(); ++i) {
      Type *argt = at;

      if(auto *argtt = dynamic_cast<TypeTuple *>(argt)) {
        argt = argtt->getElementType(i);
      }

      argts.push_back(argt);
  }

  std::vector<std::vector<size_t>> abstractionEvents;
  while(abstractionEvents.size() < ft->getArgumentCount()) abstractionEvents.push_back(std::vector<size_t>());

  bool any = true;
  while(any) {
    any = false;

    std::vector<int> abstractionPositions;
    TypeDomained *abstractionPrimary = 0;

    for(size_t i = 0; i < ft->getArgumentCount(); ++i) {
      Type *fargt = ft->getArgumentType(i);
      Type *argt = argts[i];

      int rank = ft->getArgumentRank(i);
      assert(rank);

      if(!needsAbstraction(fargt, argt, rank)) continue;

      if(!abstractionPrimary) {
        abstractionPositions.push_back(i);
        abstractionPrimary = dynamic_cast<TypeDomained *>(argt);
        assert(abstractionPrimary);
      } else if(dynamic_cast<TypeDomained *>(argt) && abstractionPrimary->canTakeDomainFrom(dynamic_cast<TypeDomained *>(argt))) {
        int tmp = abstractionPositions.front();
        abstractionPositions.push_back(tmp);
        abstractionPositions.front() = i;

        abstractionPrimary = dynamic_cast<TypeDomained *>(argt);
        assert(abstractionPrimary);
      } else if(dynamic_cast<TypeDomained *>(argt) && dynamic_cast<TypeDomained *>(argt)->canTakeDomainFrom(abstractionPrimary)) {
        abstractionPositions.push_back(i);
      }
    }

    if(!abstractionPositions.empty()) {
      for(auto i = abstractionPositions.begin(); i != abstractionPositions.end(); ++i) {
        auto atd = dynamic_cast<TypeDomained *>(argts[*i]);
        assert(atd);

        argts[*i] = atd->getReturnType();
        abstractionEvents[*i].push_back(unloopedDomains.size());
      }

      unloopedDomains.push_back(abstractionPrimary);
      any = true;
    }
  }

  type = ft->getReturnType();

  //std::cout << "number of unlooped domains: " << unloopedDomains.size() << std::endl;

  for(auto i = unloopedDomains.rbegin(); i != unloopedDomains.rend(); ++i) {
    type = (*i)->generate(type);
//    std::cout << "unlooping over: " << std::endl;
//    std::cout << type->dump(0) << std::endl;
  }

  //std::cout << "determined type (function): " << std::endl;
  //std::cout << function->getType()->dump(0) << std::endl;
  //std::cout << "determined type (argument): " << std::endl;
  //std::cout << argument->getType()->dump(0) << std::endl;
  //std::cout << "determined type (application): " << std::endl;
  //std::cout << type->dump(0) << std::endl;
  //std::cout << "requested type (application): " << std::endl;
  //std::cout << t->dump(0) << std::endl;

  if(type->canConvertTo(t) && !type->isConcrete()) {
    // determine requested domains, put them into a stack
    std::vector<TypeDomained *> requestedDomains;
    Type *reqt = t;
    for(auto i = unloopedDomains.begin(); i != unloopedDomains.end(); ++i) {
      auto reqtd = dynamic_cast<TypeDomained *>(reqt);
      assert(reqtd);
      requestedDomains.push_back(reqtd);
      reqt = reqtd->getReturnType();
    }

    // reapply that stack to the function argument types
    std::vector<Type *> argtypes;
    for(size_t i = 0; i < ft->getArgumentCount(); ++i) {
      argtypes.push_back(ft->getArgumentType(i));
    }

    for(int i = requestedDomains.size() - 1; i >= 0; --i) {
      for(size_t j = 0; j < ft->getArgumentCount(); ++j) {
        if(std::find(abstractionEvents[j].begin(), abstractionEvents[j].end(), i) != abstractionEvents[j].end()) {
          argtypes[j] = requestedDomains[i]->generate(argtypes[j]);
        }
      }
    }

    // puzzle the result into a tuple type
    if(ft->getArgumentCount() != 1) {
      TypeTuple *reqarg = new TypeTuple();
      for(auto i = argtypes.begin(); i != argtypes.end(); ++i) {
        reqarg->addElementType(*i);
      }

      // std::cout << "determined requested argument type:" << std::endl;
      // std::cout << reqarg->dump(0) << std::endl;

      argument->assignUnresolvedTypes(reqarg);
    } else {
      argument->assignUnresolvedTypes(argtypes[0]);
    }
  }

  // std::cout << "determined type (argument), after context push: " << std::endl;
  // std::cout << argument->getType()->dump(0) << std::endl;

  return type->canConvertTo(t);
}

void NodeExprApply::compile(Assembly &assembly) {
  if(auto ft = dynamic_cast<TypeFunction *>(function->getType())) {
    if(auto *argt = dynamic_cast<TypeTuple *>(argument->getType())) {
      Register64 *abiArgs[] = { rdi(), rsi(), rdx(), rcx(), r8(), r9() };
      assert(argt);

      assembly.add(push(rcx()));
      assembly.add(push(rdx()));
      assembly.add(push(rsi()));
      assembly.add(push(rdi()));
      assembly.add(push(r8()));
      assembly.add(push(r9()));
      assembly.add(push(r10()));
      assembly.add(push(r11()));
      assembly.add(cld());
      argument->compile(assembly);
      int32_t offset = 0;
      for(size_t i = 0; i < ft->getArgumentCount(); ++i) {
        assert(i < 6); // TODO: further arguments go onto stack

        Type *argti = argt->getElementType(i);
        // TODO: what about non-integer types?
        // TODO: actually do type conversion here
        assert(argti->canConvertTo(ft->getArgumentType(i)));
        assembly.add(move(memory(offset, rax()), abiArgs[i]));
        // TODO: WTF?
        offset += argti->getSize();
      }
      function->compile(assembly);
      assembly.add(call(rax()));
      assembly.add(pop(r11()));
      assembly.add(pop(r10()));
      assembly.add(pop(r9()));
      assembly.add(pop(r8()));
      assembly.add(pop(rdi()));
      assembly.add(pop(rsi()));
      assembly.add(pop(rdx()));
      assembly.add(pop(rcx()));
    } else if(ft->getArgumentCount() == 1) {
      assembly.add(push(rcx()));
      assembly.add(push(rdx()));
      assembly.add(push(rsi()));
      assembly.add(push(rdi()));
      assembly.add(push(r8()));
      assembly.add(push(r9()));
      assembly.add(push(r10()));
      assembly.add(push(r11()));
      assembly.add(cld());
      argument->compile(assembly);
      argument->getType()->convertTo(ft->getArgumentType(0), assembly);
      // TODO: what about non-integer types?
      assembly.add(move(rax(), rdi()));
      function->compile(assembly);
      assembly.add(call(rax()));
      assembly.add(pop(r11()));
      assembly.add(pop(r10()));
      assembly.add(pop(r9()));
      assembly.add(pop(r8()));
      assembly.add(pop(rdi()));
      assembly.add(pop(rsi()));
      assembly.add(pop(rdx()));
      assembly.add(pop(rcx()));
      return;
    } else {
      assert(false);
      compileError("type resolution fucked up while applying " +
          function->dump(0));
    }
  } else if(auto lt = dynamic_cast<TypeLoopable *>(function->getType())) {
    assembly.add(push(rcx()));
    argument->compile(assembly);
    assembly.add(move(rax(), rcx()));
    function->compile(assembly);
    lt->dereference(assembly);
    assembly.add(pop(rcx()));
  } else {
    assert(false);
  }
}

void NodeExprApply::compileL(Assembly &) {
  assert(false);
}

std::string NodeExprApply::dump(int i) {
  return indent(i) + "NodeExprApply\n"
    + function->dump(i + 2) + "\n"
    + argument->dump(i + 2);
}

void NodeExprProjection::resolveSymbols(SymbolTable *) {
  // nothing to do
}

void NodeExprProjection::rewriteFunctionApplications(NodeExpr **) {
  // nothing to do
}

bool NodeExprProjection::assignUnresolvedTypes(Type *t) {
  assert(level == 0);
  // std::cout << "Proj::assign" << t->dump(0) << std::endl;

  type = (new TypeFunction())
    ->setReturnType(Type::any);

  for(size_t i = 0; i <= pos; ++i) {
    type->addArgument(Type::none, 1);
  }

  auto tf = dynamic_cast<TypeFunction *>(t);
  if(tf) {
    if(tf->getArgumentCount() > pos) {
      auto rett = Type::min(tf->getArgumentType(pos), tf->getReturnType());

      type = (new TypeFunction())
        ->setReturnType(rett);

      for(size_t i = 0; i < tf->getArgumentCount(); ++i) {
        if(i == pos) {
          type->addArgument(rett, 1);
        } else {
          type->addArgument(tf->getArgumentType(i), 1);
        }
      }
    }
  }

  return type->canConvertTo(t);
}

void *projection0_0(void *v) { return v; }
void *projection1_0(void *, void *v) { return v; }
void *projection2_0(void *, void *, void *v) { return v; }
void *projection3_0(void *, void *, void *, void *v) { return v; }
void *projection4_0(void *, void *, void *, void *, void *v) { return v; }
void *projection5_0(void *, void *, void *, void *, void *, void *v) { return v; }

void NodeExprProjection::compile(Assembly &assembly) {
  assert(level == 0);

  switch(pos) {
    case 0: assembly.add(move(
      reinterpret_cast<uint64_t>(&projection0_0), rax())); break;
    case 1: assembly.add(move(
      reinterpret_cast<uint64_t>(&projection1_0), rax())); break;
    case 2: assembly.add(move(
      reinterpret_cast<uint64_t>(&projection2_0), rax())); break;
    case 3: assembly.add(move(
      reinterpret_cast<uint64_t>(&projection3_0), rax())); break;
    case 4: assembly.add(move(
      reinterpret_cast<uint64_t>(&projection4_0), rax())); break;
    case 5: assembly.add(move(
      reinterpret_cast<uint64_t>(&projection5_0), rax())); break;
    default: assert(false);
  }
}

void NodeExprProjection::compileL(Assembly &) {
  assert(false);
}

Type *NodeExprProjection::getType() {
  return type;
}

std::string NodeExprProjection::dump(int i) {
  std::ostringstream str;
  str << indent(i) << "Projection: (" << pos << ", " << level << ")";
  return str.str();
}

void NodeExprLoop::rewriteDeclarations(SymbolTable *st, NodeExpr **) {
  if(argId) return;

  argument->rewriteDeclarations(st, &argument);
  argument->resolveSymbols(st);
  argument->assignUnresolvedTypes(Type::any);
  argument->rewriteFunctionApplications(&argument);

  targetName = createUniqueIdentifier();

  st->addVar(argName, argument->getType());

  domain->rewriteDeclarations(st, &domain);
  domain->resolveSymbols(st);
  domain->assignUnresolvedTypes(Type::any);
  domain->rewriteFunctionApplications(&domain);

  auto domType = dynamic_cast<TypeLoopable *>(domain->getType());
  assert(domType);

  st->addVar(keyName, domType->getArgumentType(0));

  expr->rewriteDeclarations(st, &expr);
  expr->resolveSymbols(st);
  expr->assignUnresolvedTypes(Type::any);
  expr->rewriteFunctionApplications(&expr);

  st->addVar(targetName, domType->generate(expr->getType()));

  argId = new NodeIdentifier(argName);
  keyId = new NodeIdentifier(keyName);
  targetId = new NodeIdentifier(targetName);
}

void NodeExprLoop::resolveSymbols(SymbolTable *st) {
  argId->resolveSymbols(st);
  keyId->resolveSymbols(st);
  targetId->resolveSymbols(st);

  argument->resolveSymbols(st);
  domain->resolveSymbols(st);
  expr->resolveSymbols(st);
}

void NodeExprLoop::rewriteFunctionApplications(NodeExpr **) {
  argument->rewriteFunctionApplications(&argument);
  domain->rewriteFunctionApplications(&domain);
  expr->rewriteFunctionApplications(&expr);
}

bool NodeExprLoop::assignUnresolvedTypes(Type *t) {
  if(auto lt = dynamic_cast<TypeLoopable *>(t)) {
    argument->assignUnresolvedTypes(Type::any);
    domain->assignUnresolvedTypes(Type::any);
    expr->assignUnresolvedTypes(lt->getReturnType());
  } else {
    argument->assignUnresolvedTypes(Type::any);
    domain->assignUnresolvedTypes(Type::any);
    expr->assignUnresolvedTypes(Type::any);
  }

  auto dt = dynamic_cast<TypeLoopable *>(domain->getType());
  return dt && dt->generate(expr->getType())->canConvertTo(t);
}

void NodeExprLoop::compile(Assembly &assembly) {
  TypeLoopable *dt = dynamic_cast<TypeLoopable *>(domain->getType());
  assert(dt);
  Type *result = expr->getType();

  // TODO
  assert(argument->getType()->getSize() == 8);
  assert(dt->getSize() == 8);

  assembly.add(push(rbx()));
  assembly.add(push(rcx()));
  assembly.add(push(rdx()));

  argId->compileL(assembly);
  assembly.add(move(rax(), rbx()));
  argument->compile(assembly);
  assembly.add(move(rax(), memory(rbx())));

  targetId->compileL(assembly);
  assembly.add(move(rax(), rdx()));
  domain->compile(assembly);
  dt->loopGenerate(assembly, result);
  assembly.add(move(rax(), memory(rdx())));

  domain->compile(assembly);
  assembly.add(move(rax(), rbx()));

  void *data = dt->loopBegin(assembly, result);
  keyId->compileL(assembly);
  assembly.add(move(rcx(), memory(rax())));

  targetId->compile(assembly);
  assembly.add(move(rax(), rdx()));
  keyId->compile(assembly);
  assembly.add(move(rax(), rcx()));
  expr->compile(assembly);
  dt->loopSaveResult(assembly, result);

  assembly.add(move(memory(rbx()), rax()));
  dt->loopStep(assembly, result);

  assembly.add(move(memory(rbx()), rax()));

  domain->compile(assembly);
  dt->loopEnd(assembly, result, data);

  assembly.add(pop(rdx()));
  assembly.add(pop(rcx()));
  assembly.add(pop(rbx()));

  targetId->compile(assembly);
}

void NodeExprLoop::compileL(Assembly &) {
  assert(false);
}

Type *NodeExprLoop::getType() {
  if(!type) {
    TypeLoopable *tl = dynamic_cast<TypeLoopable *>(domain->getType());
    assert(tl);

    type = tl->generate(expr->getType());
  }

  return type;
}

std::string NodeExprLoop::dump(int i) {
  std::ostringstream str;
  str << indent(i) <<
    "NodeExprLoop (argument: " << argName << ", key: " << keyName << ", target: " << targetName << ")\n"
    << argument->dump(i + 2) << "\n"
    << domain->dump(i + 2) << "\n"
    << expr->dump(i + 2);
  return str.str();
}

void TreeNode::compileError(const std::string &str) {
  throw std::runtime_error(str);
}

std::string TreeNode::indent(int i) {
  std::string ret = "";
  while(i--) ret += " ";
  return ret;
}

std::string TreeNode::dump(int i) {
  return indent(i) + typeid(*this).name();
}

void NodeExprBinarySimple::rewriteDeclarations(SymbolTable *syms, NodeExpr **parent) {
  *parent = new NodeExprApply(implementation(), new NodeExprTuple(lhs, rhs));

  (*parent)->rewriteDeclarations(syms, parent);
}

NodeExpr *NodeExprBinarySimple::implementation() {
  assert(false); // TODO: subclass should have done this
}

bool NodeExprBinarySimple::NodeExprConstantFunction::assignUnresolvedTypes(Type *t) {
  return type->canConvertTo(t);
}

static int32_t addition_int32_t(int32_t a, int32_t b) {
  return a + b;
}

NodeExpr *NodeExprAdd::implementation() {
  return 
    new (class _: public NodeExprConstantFunction {
      public:
        _(): NodeExprConstantFunction((new TypeFunction())
          ->setReturnType(Type::sint32)
          ->addArgument(Type::sint32, 1)
          ->addArgument(Type::sint32, 1)) { }
        void compile(Assembly &assembly) {
          assembly.add(move(reinterpret_cast<uint64_t>(&addition_int32_t), rax()));
        }
        std::string dump(int i) { return indent(i) + "α + β"; }
    }) ();
}

void NodeExprDeclaration::rewriteDeclarations(SymbolTable *st, NodeExpr **) {
  st->addVar(name->get(), type);
}

void NodeExprDeclaration::resolveSymbols(SymbolTable *st) {
  name->resolveSymbols(st);
}

void NodeExprDeclaration::rewriteFunctionApplications(NodeExpr **) {
  // no functions in here
}

bool NodeExprDeclaration::assignUnresolvedTypes(Type *t) {
  return getType()->canConvertTo(t);
}

void NodeExprDeclaration::compile(Assembly &) {
  // TODO: initialize variable here
  // name->compileL(assembly);
  // type->compile(assembly);
}

void NodeExprDeclaration::compileL(Assembly &assembly) {
  name->compileL(assembly);
}

std::string NodeExprDeclaration::dump(int i) {
  return indent(i) + "ExprDeclaration\n" + type->dump(i + 2) + "\n" + name->dump(i + 2);
}

void NodeExprAssign::resolveSymbols(SymbolTable *st) {
  lhs->resolveSymbols(st);
  rhs->resolveSymbols(st);
}

void NodeExprAssign::rewriteDeclarations(SymbolTable *st, NodeExpr **) {
  lhs->rewriteDeclarations(st, &lhs);
  rhs->rewriteDeclarations(st, &rhs);

  syms = st;
}

// TYPE TODO
bool NodeExprAssign::assignUnresolvedTypes(Type *t) {
  // TODO: fix if one side is function
  lhs->assignUnresolvedTypes(t);
  type = lhs->getType();
  rhs->assignUnresolvedTypes(type);

  if(!type) {
    type = rhs->getType();
    lhs->assignUnresolvedTypes(rhs->getType());
  }

  assert(type);

  return type->canConvertTo(t);
}

void NodeExprAssign::rewriteFunctionApplications(NodeExpr **parent) {
  rhs->rewriteFunctionApplications(&rhs);
  lhs->rewriteFunctionApplications(&lhs);

  if(auto ft = dynamic_cast<TypeFunction *>(rhs->getType())) {
    if(!dynamic_cast<TypeFunction *>(lhs->getType())) {
      NodeExprList *replacementArguments = new NodeExprList();
      NodeExprList *replacementCall = new NodeExprList();

      for(size_t j = 0; j < ft->getArgumentCount(); ++j) {
        auto id = createUniqueIdentifier();

        replacementArguments->add(new NodeExprDeclaration(ft->getArgumentType(j), new NodeIdentifier(id)));
        replacementCall->add(new NodeIdentifier(id));
      }

      assert(lhs->getType());

      (*parent) = new NodeTypedLambda(new NodeExprTuple(replacementArguments), lhs->getType(),
          new NodeStatementExpr(new NodeExprAssign(lhs, new NodeExprApply(rhs, new NodeExprTuple(replacementCall)))));

      assert(syms);
      (*parent)->rewriteDeclarations(syms, parent);
      (*parent)->resolveSymbols(syms);
      (*parent)->assignUnresolvedTypes(getType());
      (*parent)->rewriteFunctionApplications(parent);
      return;
    }
  }
}

void NodeExprAssign::compile(Assembly &assembly) {
  if(*lhs->getType() != *rhs->getType()) {
    std::cerr << "LHS: \n" << lhs->getType()->dump(0) << std::endl;
    std::cerr << "RHS: \n" << rhs->getType()->dump(0) << std::endl;
    std::cerr << "LHS: \n" << lhs->dump(0) << std::endl;
    std::cerr << "RHS: \n" << rhs->dump(0) << std::endl;
  }

  assert(*lhs->getType() == *rhs->getType());

  assembly.add(push(rbx()));
  lhs->compileL(assembly);
  assembly.add(move(rax(), rbx()));
  rhs->compile(assembly);
  assembly.add(move(rax(), memory(rbx())));
  assembly.add(pop(rbx()));
}

void NodeExprAssign::compileL(Assembly &assembly) {
  // TODO: do the function application rewriting
  assert(*lhs->getType() == *rhs->getType());

  assembly.add(push(rbx()));
  lhs->compileL(assembly);
  assembly.add(move(rax(), rbx()));
  rhs->compile(assembly);
  assembly.add(move(rax(), memory(rbx())));
  assembly.add(move(rbx(), rax()));
  assembly.add(pop(rbx()));
}

void NodeTypeFunction::rewriteDeclarations(SymbolTable *st, NodeExpr **parent) {
  arg->rewriteDeclarations(st, &arg);
  ret->rewriteDeclarations(st, &ret);

  Type *argt = dynamic_cast<Type *>(arg);
  if(!argt) compileError("function type needs type expressions on left side");
  Type *rett = dynamic_cast<Type *>(ret);
  if(!rett) compileError("function type needs type expressions on right side");

  TypeFunction *func = new TypeFunction();
  func->setReturnType(rett);

  if(TypeTuple *args = dynamic_cast<TypeTuple *>(argt)) {
    assert(args);

    for(size_t i = 0; i < args->getTupleWidth(); ++i) {
      func->addArgument(args->getElementType(i), 1);
    }
  } else {
    func->addArgument(argt, 1);
  }

  *parent = func;
}

NodeTypedLambda::NodeTypedLambda(NodeExprTuple *args, NodeExpr *ret, NodeStatement *body)
  : args(args), ret(ret), body(body), type(0) {
  assert(args);
  assert(ret);
  assert(body);
}

void NodeTypedLambda::rewriteDeclarations(SymbolTable *st, NodeExpr **) {
  localSymbols = new SymbolTableClosure(st);
  
  args->rewriteDeclarations(localSymbols, static_cast<NodeExpr **>(0));
  ret->rewriteDeclarations(localSymbols, &ret);
  body->rewriteDeclarations(localSymbols);
}

void NodeTypedLambda::resolveSymbols(SymbolTable *) {
  body->resolveSymbols(localSymbols);

  localSymbols->setStackOffsets();
  localSymbols->setHeapOffsets();
}

void NodeTypedLambda::rewriteFunctionApplications(NodeExpr **) {
  body->rewriteFunctionApplications();
}

bool NodeTypedLambda::assignUnresolvedTypes(Type *) {
  type = dynamic_cast<TypeFunction *>(getType());
  assert(type);

  return body->assignUnresolvedTypes(type->getReturnType());
}

void NodeTypedLambda::compile(Assembly &assembly) {
  Assembly code;

  if(localSymbols->getHeapVariableSize()) {
    // get data area address
    code.add(pop(r8()));
    code.add(add(-6, r8()));
  }

  code.add(enter(-localSymbols->getStackSize()));

  Register64 *abiArgs[] = { rdi(), rsi(), rdx(), rcx(), r8(), r9() };

  NodeExprTuple *argst = dynamic_cast<NodeExprTuple *>(args);
  assert(argst);

  for(size_t i = 0; i < argst->getElements().size(); ++i) {
    assert(i < 6);

    NodeExprDeclaration *decl =
      dynamic_cast<NodeExprDeclaration *>(argst->getElements()[i]);
    assert(decl);
    Symbol *sym = localSymbols->resolve(decl->getName()->get());
    SymbolLocalVariable *param = dynamic_cast<SymbolLocalVariable *>(sym);
    assert(param);

    code.add(move(abiArgs[i], memory(param->getStackOffset(), rbp())));
  }

  body->compile(code);

  code.add(leave());
  code.add(retnear());
  void *binary = code.assemble();

  if(!localSymbols->getHeapVariableSize()) {
    assembly.add(move(reinterpret_cast<uint64_t>(binary), rax()));
  } else {
    alloc(assembly, localSymbols->getHeapVariableSize());

    static void *padBinary = 0;
    if(!padBinary) {
      Assembly pad;
      // the call takes 6 bytes, we want stuff to be aligned though
      pad.add(call(memory(2, rip())));
      padBinary = pad.assemble();
    }

    // install jump pad
    assembly.add(push(rbx()));
    assembly.add(move(rax(), rbx()));
    assembly.add(move(*reinterpret_cast<uint64_t *>(padBinary), rax()));
    assembly.add(move(rax(), memory(rbx())));
    assembly.add(move(reinterpret_cast<uint64_t>(binary), rax()));
    assembly.add(move(rax(), memory(8, rbx())));

    // copy bound variables
    auto syms = localSymbols->getSymbols();
    for(auto i = syms.begin(); i != syms.end(); ++i) {
      if(auto var = dynamic_cast<SymbolClosureVariable *>(i->second)) {
        compileRValue(assembly, var->getParentSymbol());
        assembly.add(move(rax(), memory(var->getHeapOffset(), rbx())));
      }
    }

    assembly.add(move(rbx(), rax()));
    assembly.add(pop(rbx()));
  }
}

void NodeTypedLambda::compileL(Assembly &) {
  // TODO: enable writable code blocks one day
  assert(false);
}

Type *NodeTypedLambda::getType() {
  if(!type) {
    type = new TypeFunction();

    Type *retType = dynamic_cast<Type *>(ret);
    assert(retType);

    type->setReturnType(retType);

    NodeExprTuple *argst = dynamic_cast<NodeExprTuple *>(args);
    assert(argst);

    const std::vector<NodeExpr *> &params = argst->getElements();
    for(auto i = params.begin(); i != params.end(); ++i) {
      NodeExprDeclaration *decl = dynamic_cast<NodeExprDeclaration *>(*i);
      type->addArgument(decl->getType(), 1);
    }
  }

  return type;
}

std::string NodeTypedLambda::dump(int i) {
  std::ostringstream s;

  s << indent(i) << "λ(\n"
    << args->dump(i + 2) << "\n"
    << indent(i) << ")→\n"
    << ret->dump(i + 2) << "\n"
    << indent(i) << "body:\n"
    << body->dump(i + 2);

  return s.str();
}

void NodeExprUnaryMinus::compile(Assembly &assembly) {
  arg->compile(assembly);
  assembly.add(neg(rax()));
}

unsigned long TreeNode::creations = 0;
