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

void NodeStatementExpr::assignUnresolvedTypes(Type *t) {
  expr->assignUnresolvedTypes(t);
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

void NodeStatementWhile::assignUnresolvedTypes(Type *t) {
  condition->assignUnresolvedTypes(Type::boolean);
  body->assignUnresolvedTypes(t);
}

void NodeStatementWhile::compile(Assembly &assembly) {
  assert(false);
}

void NodeStatementGoto::resolveSymbols(SymbolTable *st) {
  target->resolveSymbols(st);
}

void NodeStatementGoto::rewriteFunctionApplications() {
  target->rewriteFunctionApplications(&target);
}

void NodeStatementGoto::assignUnresolvedTypes(Type *t) {
  target->assignUnresolvedTypes(t);
}

void NodeStatementGoto::compile(Assembly &assembly) {
  assert(false);
}

void NodeStatementLabel::resolveSymbols(SymbolTable *st) {
  name->resolveSymbols(st);
}

void NodeStatementLabel::rewriteFunctionApplications() {
  // no rewriting the identifier so far
}

void NodeStatementLabel::assignUnresolvedTypes(Type *t) {
  name->assignUnresolvedTypes(t);
}

void NodeStatementLabel::compile(Assembly &assembly) {
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

void NodeStatementRule::assignUnresolvedTypes(Type *t) {
  assert(false);
  lhs->assignUnresolvedTypes(0);
  rhs->assignUnresolvedTypes(0);
  mapping->assignUnresolvedTypes(0);
}

void NodeStatementRule::compile(Assembly &assembly) {
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

void NodeStatementBlock::assignUnresolvedTypes(Type *t) {
  for(auto i = statements.begin(); i != statements.end(); ++i) {
    (*i)->assignUnresolvedTypes(t);
  }
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

void NodeExprUnary::assignUnresolvedTypes(Type *t) {
  arg->assignUnresolvedTypes(t);
}

void NodeExprUnary::compile(Assembly &assembly) {
  assert(false);
}

void NodeExprUnary::compileL(Assembly &assembly) {
  assert(false);
}

Type *NodeExprUnary::getType() {
  return arg->getType();
}

std::string NodeExprUnary::dump(int i) {
  return indent(i) + op() + "\n" + arg->dump(i + 2);
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

void NodeInteger::resolveSymbols(SymbolTable *st) {
  // nothing to do
}

void NodeInteger::compile(Assembly &assembly) {
  assembly.add(move(val, rax()));
}

void NodeInteger::compileL(Assembly &assembly) {
  assert(false);
}

Type *NodeInteger::getType() {
  // TODO: use value to determine type
  return Type::sint32;
}

void NodeString::resolveSymbols(SymbolTable *st) {
  // nothing to do
}

void NodeString::compile(Assembly &assembly) {
  assert(false);
}

void NodeString::compileL(Assembly &assembly) {
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

void NodeExprTuple::assignUnresolvedTypes(Type *t) {
  if(elements.size() == 1) {
    elements[0]->assignUnresolvedTypes(t);
  } else if(auto tt = dynamic_cast<TypeTuple *>(t)) {
    for(int i = 0; i < tt->getTupleWidth(); ++i) {
      elements[i]->assignUnresolvedTypes(tt->getElementType(i));
    }
  } else {
    for(auto i = elements.begin(); i != elements.end(); ++i) {
      (*i)->assignUnresolvedTypes(Type::any);
    }

    if(!t->canConvertTo(getType())) {
      compileError("Type request could not be fulfilled: Not a tuple type.");
    }
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
    for(int i = 0; i < elements.size(); ++i) {
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

void NodeExprTuple::compileL(Assembly &assembly) {
  assert(false);
}

Type *NodeExprTuple::getType() {
  if(!type) {
    if(elements.size() == 1) {
      type = elements[0]->getType();
    } else {
      TypeTuple *typet = new TypeTuple();
      for(auto i = elements.begin(); i != elements.end(); ++i) {
        Type *et = (*i)->getType();
        if(auto ett = dynamic_cast<TypeTuple *>(et)) {
          for(int j = 0; j < ett->getTupleWidth(); ++j) {
            typet->addElementType(ett->getElementType(j));
          }
        } else {
          typet->addElementType(et);
        }
      }

      type = typet;
    }
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

    void *loopBegin(Assembly &assembly, Type *result) {
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

    void loopEnd(Assembly &assembly, Type *result, void *data) {
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

void NodeExprArray::assignUnresolvedTypes(Type *t) {
  Type *elementType = Type::any;

  if(auto at = dynamic_cast<TypeLoopable *>(t)) {
    elementType = at->getReturnType();
  }

  for(auto i = elements.begin(); i != elements.end(); ++i) {
    (*i)->assignUnresolvedTypes(elementType);
  }
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

  for(int i = 0; i < elements.size(); ++i) {
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

void NodeExprArray::compileL(Assembly &assembly) {
  assert(false);
}

Type *NodeExprArray::getType() {
  Type *inner = Type::any;

  if(elements.size()) {
    inner = elements[0]->getType();

    for(int i = 1; i < elements.size(); ++i) {
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

class RCX: public NodeExpr {
  public:
    RCX(Type *type): type(type) { }
    void rewriteDeclarations(SymbolTable *, NodeExpr **) { assert(false); }
    void resolveSymbols(SymbolTable *) { }
    void rewriteFunctionApplications(NodeExpr **) { }
    void assignUnresolvedTypes(Type *t) { }
    void compile(Assembly &assembly) {
      assembly.add(move(rcx(), rax()));
    }
    void compileL(Assembly &assembly) {
      assert(false);
    }
    Type *getType() { return type; }

  private:
    Type *type;
};

static std::string createUniqueIdentifier() {
  static unsigned long long id = 0;

  std::ostringstream str;
  str << "#" << id++;
  return str.str();
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
// FIXME: REWRITE THIS WHOLE FUNCTION
void NodeExprApply::rewriteFunctionApplications(NodeExpr **parent) {
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
      } else if(auto *atl = dynamic_cast<TypeLoopable *>(at)) {
        auto argName = createUniqueIdentifier();
        auto keyName = createUniqueIdentifier();

        *parent = new NodeExprLoop(argument, argName, keyName,
            new NodeExprApply(function, new NodeExprApply(new NodeIdentifier(argName), new NodeIdentifier(keyName))));
        (*parent)->rewriteDeclarations(syms, parent);
        (*parent)->resolveSymbols(syms);
        (*parent)->assignUnresolvedTypes(getType());
        (*parent)->rewriteFunctionApplications(parent);
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
  } else {
    if(ft->getArgumentCount() > 1) {
      compileError("overload resolution fucked up, "
          "autolooping is deeply sorry (2b): " + function->dump(0));
    }
  }

  for(int i = 0; i < ft->getArgumentCount(); ++i) {
    Type *fargt = ft->getArgumentType(i);
    Type *argt = at;
    if(TypeTuple *argtt = dynamic_cast<TypeTuple *>(argt)) {
      argt = argtt->getElementType(i);
    }

    int rank = ft->getArgumentRank(i);
    assert(rank);

    if(auto argft = dynamic_cast<TypeFunction *>(argt) && !dynamic_cast<TypeFunction *>(fargt)) {
      NodeExprList *replacementArguments = new NodeExprList();
      NodeExprList *replacementCall = new NodeExprList();

      for(int j = 0; j < ft->getArgumentCount(); ++j) {
        auto id = createUniqueIdentifier();

        if(j == i) {
          replacementArguments->add(new NodeExprDeclaration(ft->getArgumentType(i), new NodeIdentifier(id)));

          NodeExpr *fun = argument;
          if(auto *funt = dynamic_cast<NodeExprTuple *>(fun)) {
            fun = funt->getElements()[j];
          }

          replacementCall->add(new NodeExprApply(fun, new NodeIdentifier(id)));
        } else {
          replacementCall->add(new NodeExprApply(new NodeExprProjection(j, 0), argument));
        }
      }

      assert(getType());

      (*parent) = new NodeTypedLambda(new NodeExprTuple(replacementArguments), getType(),
          new NodeStatementExpr(new NodeExprApply(function, new NodeExprTuple(replacementCall))));

      assert(syms);
      (*parent)->rewriteDeclarations(syms, parent);
      (*parent)->resolveSymbols(syms);
      (*parent)->assignUnresolvedTypes(getType());
      (*parent)->rewriteFunctionApplications(parent);
      return;
    }

    if(rank > 0) {
      int depth = 0;
      while(1) {
        if(argt->canConvertTo(fargt)) {
          ++depth;
        }

        if(TypeTuple *att = dynamic_cast<TypeTuple *>(argt)) {
          if(att->getTupleWidth() == 1) {
            argt = att->getElementType(0);
          } else {
            break;
          }
        } else if(auto *atd = dynamic_cast<TypeDomained *>(argt)) {
          argt = atd->getReturnType();
        } else {
          break;
        }
      }

      if(!depth) {
        compileError("overload resolution fucked up, "
            "autolooping is deeply sorry (3): " + function->dump(0));
      }

      rank = rank - depth - 1;
    }

    argt = at;
    if(TypeTuple *argtt = dynamic_cast<TypeTuple *>(argt)) {
      argt = argtt->getElementType(i);
    }

    while(1) {
      if(argt->canConvertTo(fargt)) {
        ++rank;
      }

      if(!rank) break;

      if(TypeTuple *att = dynamic_cast<TypeTuple *>(argt)) {
        if(att->getTupleWidth() == 1) {
          argt = att->getElementType(0);
        } else {
          compileError("overload resolution fucked up, "
              "autolooping is deeply sorry (4): " + function->dump(0));
        }
      } else if(TypeLoopable *atl = dynamic_cast<TypeLoopable *>(argt)) {
        if(auto *origTuple = dynamic_cast<TypeTuple *>(at)) {
          assert(origTuple);

          TypeTuple *innerType = new TypeTuple();
          for(int j = 0; j < origTuple->getTupleWidth(); ++j) {
            if(i == j) {
              innerType->addElementType(atl->getReturnType());
            } else {
              innerType->addElementType(origTuple->getElementType(j));
            }
          }

          assert(false);
        } else {
          auto argName = createUniqueIdentifier();
          auto keyName = createUniqueIdentifier();

          *parent = new NodeExprLoop(argument, argName, keyName,
              new NodeExprApply(function, new NodeExprApply(new NodeIdentifier(argName), new NodeIdentifier(keyName))));
        }

        (*parent)->rewriteDeclarations(syms, parent);
        (*parent)->resolveSymbols(syms);
        (*parent)->assignUnresolvedTypes(getType());
        (*parent)->rewriteFunctionApplications(parent);
        // TODO: correctly handle co-looping
        return;
      } else {
        compileError("overload resolution fucked up, "
            "autolooping is deeply sorry (4): " + function->dump(0));
      }
    }
  }
}

void NodeExprApply::assignUnresolvedTypes(Type *t) {
  function->assignUnresolvedTypes(Type::any);
  argument->assignUnresolvedTypes(Type::any);

  Type *oldFt = function->getType();
  Type *oldAt = argument->getType();

  do {
    oldFt = function->getType();
    oldAt = argument->getType();

    TypeFunction *ft = new TypeFunction();

    if(auto att = dynamic_cast<TypeTuple *>(oldAt)) {
      for(int i = 0; i < att->getTupleWidth(); ++i) {
        ft->addArgument(att->getElementType(i), 1);
      }
    } else {
      ft->addArgument(oldAt, 1);
    }

    function->assignUnresolvedTypes(ft);

    TypeDomained *newFt = dynamic_cast<TypeDomained *>(function->getType());
    assert(newFt);

    if(newFt->getArgumentCount() == 1) {
      argument->assignUnresolvedTypes(newFt->getArgumentType(0));
    } else {
      TypeTuple *newAt = new TypeTuple();

      for(int i = 0; i < ft->getArgumentCount(); ++i) {
        newAt->addElementType(ft->getArgumentType(i));
      }

      argument->assignUnresolvedTypes(newAt);
    }
  } while(*oldFt != *function->getType() || *oldAt != *argument->getType());
}

void NodeExprApply::compile(Assembly &assembly) {
  if(auto ft = dynamic_cast<TypeFunction *>(function->getType())) {
    Type *at = argument->getType();

    NodeExpr *arg = argument;

    if(auto *argt = dynamic_cast<TypeTuple *>(arg->getType())) {
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
      arg->compile(assembly);
      int32_t offset = 0;
      for(int i = 0; i < ft->getArgumentCount(); ++i) {
        assert(i < 6); // TODO: further arguments go onto stack

        Type *argti = argt->getElementType(i);
        // TODO: what about non-integer types?
        assembly.add(move(memory(offset, rax()), abiArgs[i]));
        // TODO: WTF?
        argti->convertTo(ft->getArgumentType(i), assembly);
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
      arg->compile(assembly);
      arg->getType()->convertTo(ft->getArgumentType(0), assembly);
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

void NodeExprApply::compileL(Assembly &assembly) {
  assert(false);
}

Type *NodeExprApply::getType() {
  Type *t = function->getType();

  if(auto tt = dynamic_cast<TypeTuple *>(t)) {
    assert(tt->getTupleWidth() != 1);
    assert(false); // TODO: actually there should be a function generating a tuple result
  }

  auto dt = dynamic_cast<TypeDomained *>(t);
  assert(dt);

  return dt->getReturnType();
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

void NodeExprProjection::assignUnresolvedTypes(Type *t) {
  assert(level == 0);

  auto tf = dynamic_cast<TypeFunction *>(t);
  if(tf) {
    Type *argt = Type::none;
    bool posResolved = false;

    if(tf->getArgumentCount() <= pos) {
      argt = tf->getArgumentType(0);
    } else {
      argt = tf->getArgumentType(pos);
      posResolved = true;
    }

    while(auto loop = dynamic_cast<TypeLoopable *>(argt)) {
      argt = loop->getReturnType();
    }
        
    if(auto argtt = dynamic_cast<TypeTuple *>(argt)) {
      type = new TypeFunction();

      for(int i = 0; i < argtt->getTupleWidth(); ++i) {
        type->addArgument(argtt->getElementType(i), 1);
      }

      type->setReturnType(argtt->getElementType(pos));
    } else {
      TypeTuple *attempt = new TypeTuple();
      for(int i = 0; i < pos; ++i) {
        attempt->addElementType(Type::none);
      }

      if(argt->canConvertTo(attempt)) {
        type = (new TypeFunction())
          ->setReturnType(Type::any);

        for(int i = 0; i < pos; ++i) {
          type->addArgument(Type::any, 1);
        }
      } else {
        assert(posResolved);

        type = (new TypeFunction())
          ->addArgument(argt, 1)
          ->setReturnType(argt);
      }
    }
  } else {
    type = (new TypeFunction())
      ->setReturnType(t);

    for(int i = 0; i < pos - 1; ++i) {
      type->addArgument(Type::any, 1);
    }

    type->addArgument(t, 1);
  }
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

void NodeExprProjection::compileL(Assembly &assembly) {
  assert(false);
}

Type *NodeExprProjection::getType() {
  return type;
}

void NodeExprLoop::rewriteDeclarations(SymbolTable *st, NodeExpr **) {
  auto contType = dynamic_cast<TypeLoopable *>(container->getType());
  assert(contType);

  if(!contId) {
    targetName = createUniqueIdentifier();

    st->addVar(contName, contType);
    st->addVar(keyName, contType->getReturnType());
    st->addVar(targetName, contType->generate(expr->getType()));

    contId = new NodeIdentifier(contName);
    keyId = new NodeIdentifier(keyName);
    targetId = new NodeIdentifier(targetName);
  }
}

void NodeExprLoop::resolveSymbols(SymbolTable *st) {
  contId->resolveSymbols(st);
  keyId->resolveSymbols(st);
  targetId->resolveSymbols(st);

  container->resolveSymbols(st);
  expr->resolveSymbols(st);
}

void NodeExprLoop::rewriteFunctionApplications(NodeExpr **parent) {
  container->rewriteFunctionApplications(&container);
  expr->rewriteFunctionApplications(&expr);
}

// TYPE TODO: make it a real fixpoint thing
void NodeExprLoop::assignUnresolvedTypes(Type *t) {
  if(auto lt = dynamic_cast<TypeLoopable *>(t)) {
    container->assignUnresolvedTypes(Type::any);
    expr->assignUnresolvedTypes(lt->getReturnType());
  } else {
    container->assignUnresolvedTypes(Type::any);
    expr->assignUnresolvedTypes(Type::any);
  }
}

void NodeExprLoop::compile(Assembly &assembly) {
  TypeLoopable *ct = dynamic_cast<TypeLoopable *>(container->getType());
  assert(ct);
  Type *result = expr->getType();

  // TODO
  assert(container->getType()->getSize() == 8);

  assembly.add(push(rbx()));
  assembly.add(push(rcx()));
  assembly.add(push(rdx()));

  contId->compileL(assembly);
  assembly.add(move(rax(), rbx()));
  container->compile(assembly);
  assembly.add(move(rax(), memory(rbx())));

  targetId->compileL(assembly);
  assembly.add(move(rax(), rdx()));
  container->compile(assembly);
  ct->loopGenerate(assembly, result);
  assembly.add(move(rax(), memory(rdx())));

  assembly.add(move(memory(rbx()), rax()));
  void *data = ct->loopBegin(assembly, result);
  keyId->compileL(assembly);
  assembly.add(move(rcx(), memory(rax())));

  targetId->compile(assembly);
  assembly.add(move(rax(), rdx()));
  keyId->compile(assembly);
  assembly.add(move(rax(), rcx()));
  expr->compile(assembly);
  ct->loopSaveResult(assembly, result);

  assembly.add(move(memory(rbx()), rax()));
  ct->loopStep(assembly, result);

  assembly.add(move(memory(rbx()), rax()));
  ct->loopEnd(assembly, result, data);

  assembly.add(pop(rdx()));
  assembly.add(pop(rcx()));
  assembly.add(pop(rbx()));

  targetId->compile(assembly);

//  } else {
//    TypeTuple *tt = dynamic_cast<TypeTuple *>(container->getType());
//    assert(tt);
//    TypeLoopable *ct = dynamic_cast<TypeLoopable *>(tt->getElementType(tupleIndex));
//    assert(ct);
//    Type *result = expr->getType();
//
//    TypeTuple *nt = new TypeTuple();
//    for(int i = 0; i < tt->getTupleWidth(); ++i) {
//      if(i == tupleIndex) {
//        nt->addElementType(ct->getReturnType());
//      } else {
//        nt->addElementType(tt->getElementType(i));
//      }
//    }
//
//    container->compile(assembly);
//    assembly.add(push(rsi()));
//    assembly.add(move(rax(), rsi()));
//    assembly.add(move(memory(tt->getElementOffset(tupleIndex), rsi()), rax()));
//    void *data = ct->loopBegin(assembly, result);
//    ct->loopStep(assembly, result, data);
//
//    alloc(assembly, nt->getHeapSize());
//
//    for(int i = 0; i < tt->getTupleWidth(); ++i) {
//      if(i == tupleIndex) {
//        switch(tt->getElementType(i)->getSize()) {
//          case 8:
//            assembly.add(move(rcx(), memory(nt->getElementOffset(i), rax())));
//            break;
//          default:
//            assert(false);
//        }
//      } else {
//        switch(tt->getElementType(i)->getSize()) {
//          case 8:
//            assembly.add(push(rdx()));
//            assembly.add(move(memory(tt->getElementOffset(i), rsi()), rdx()));
//            assembly.add(move(rdx(), memory(nt->getElementOffset(i), rax())));
//            assembly.add(pop(rdx()));
//            break;
//          default:
//            assert(false);
//        }
//      }
//    }
//    
//    assembly.add(move(rax(), rcx()));
//    expr->compile(assembly);
//    ct->loopEnd(assembly, result, data);
//
//    assembly.add(pop(rsi()));
//  }
}

void NodeExprLoop::compileL(Assembly &assembly) {
  assert(false);
}

Type *NodeExprLoop::getType() {
  if(!type) {
    TypeLoopable *tl = dynamic_cast<TypeLoopable *>(container->getType());
    assert(tl);

    type = tl->generate(expr->getType());
  }

  return type;
}

std::string NodeExprLoop::dump(int i) {
  std::ostringstream str;
  str << indent(i) <<
    "NodeExprLoop (container: " << contName << ", key: " << keyName << ", target: " << targetName << ")\n"
    << container->dump(i + 2) << "\n"
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

void NodeExprDeclaration::assignUnresolvedTypes(Type *) {
  // there is no unclearness of types in here
}

void NodeExprDeclaration::compile(Assembly &assembly) {
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
void NodeExprAssign::assignUnresolvedTypes(Type *t) {
  // TODO: fix if one side is function
  lhs->assignUnresolvedTypes(t);
  type = lhs->getType();
  rhs->assignUnresolvedTypes(type);

  if(!type) {
    type = rhs->getType();
    lhs->assignUnresolvedTypes(rhs->getType());
  }

  assert(type);
}

void NodeExprAssign::rewriteFunctionApplications(NodeExpr **parent) {
  rhs->rewriteFunctionApplications(&rhs);
  lhs->rewriteFunctionApplications(&lhs);

  if(auto ft = dynamic_cast<TypeFunction *>(rhs->getType())) {
    if(!dynamic_cast<TypeFunction *>(lhs->getType())) {
      NodeExprList *replacementArguments = new NodeExprList();
      NodeExprList *replacementCall = new NodeExprList();

      for(int j = 0; j < ft->getArgumentCount(); ++j) {
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

    for(int i = 0; i < args->getTupleWidth(); ++i) {
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

void NodeTypedLambda::rewriteDeclarations(SymbolTable *st, NodeExpr **parent) {
  localSymbols = new SymbolTableClosure(st);
  
  args->rewriteDeclarations(localSymbols, static_cast<NodeExpr **>(0));
  ret->rewriteDeclarations(localSymbols, &ret);
  body->rewriteDeclarations(localSymbols);
}

void NodeTypedLambda::resolveSymbols(SymbolTable *st) {
  body->resolveSymbols(localSymbols);

  localSymbols->setStackOffsets();
  localSymbols->setHeapOffsets();
}

void NodeTypedLambda::rewriteFunctionApplications(NodeExpr **) {
  body->rewriteFunctionApplications();
}

void NodeTypedLambda::assignUnresolvedTypes(Type *t) {
  type = dynamic_cast<TypeFunction *>(getType());
  assert(type);

  body->assignUnresolvedTypes(type->getReturnType());
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

  for(int i = 0; i < argst->getElements().size(); ++i) {
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

void NodeTypedLambda::compileL(Assembly &assembly) {
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
