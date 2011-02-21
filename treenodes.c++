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

void NodeStatementExpr::assignUnresolvedTypes() {
  expr->assignUnresolvedTypes();
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

void NodeStatementWhile::assignUnresolvedTypes() {
  condition->assignUnresolvedTypes();
  body->assignUnresolvedTypes();
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

void NodeStatementGoto::assignUnresolvedTypes() {
  target->assignUnresolvedTypes();
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

void NodeStatementLabel::assignUnresolvedTypes() {
  name->assignUnresolvedTypes();
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

void NodeStatementRule::assignUnresolvedTypes() {
  lhs->assignUnresolvedTypes();
  rhs->assignUnresolvedTypes();
  mapping->assignUnresolvedTypes();
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

void NodeStatementBlock::assignUnresolvedTypes() {
  for(auto i = statements.begin(); i != statements.end(); ++i) {
    (*i)->assignUnresolvedTypes();
  }
}

void NodeStatementBlock::compile(Assembly &assembly) {
  for(auto i = statements.begin(); i != statements.end(); ++i) {
    (*i)->compile(assembly);
  }
}

void NodeExprBinary::rewriteDeclarations(SymbolTable *st, NodeExpr **) {
  lhs->rewriteDeclarations(st, &lhs);
  rhs->rewriteDeclarations(st, &rhs);
}

void NodeExprBinary::resolveSymbols(SymbolTable *st) {
  lhs->resolveSymbols(st);
  rhs->resolveSymbols(st);

  syms = st;
}

void NodeExprBinary::rewriteFunctionApplications(NodeExpr **) {
  lhs->rewriteFunctionApplications(&lhs);
  rhs->rewriteFunctionApplications(&rhs);
}

void NodeExprBinary::assignUnresolvedTypes() {
  lhs->assignUnresolvedTypes();
  rhs->assignUnresolvedTypes();
}

void NodeExprBinary::compile(Assembly &assembly) {
  assert(false);
}

void NodeExprBinary::compileL(Assembly &assembly) {
  assert(false);
}

NodeExprFunction *NodeExprBinary::assignType(Type *type) {
  assert(false);
}

Type *NodeExprBinary::getType() {
  Type *lt = lhs->getType();
  Type *rt = rhs->getType();
  if(rt->canConvertTo(lt)) {
    return lt;
  } else if(lt->canConvertTo(rt)) {
    return rt;
  } else if(auto *ltf = dynamic_cast<TypeFunction *>(lt)) {
    assert(false);
  } else if(auto *rtf = dynamic_cast<TypeFunction *>(rt)) {
    assert(false);
  } else if(auto *ltf = dynamic_cast<TypeFunctionSet *>(lt)) {
    return new TypeNodeBackedFunctionSet(this);
  } else if(auto *rtf = dynamic_cast<TypeFunctionSet *>(rt)) {
    return new TypeNodeBackedFunctionSet(this);
  } else {
    // TODO: do real lowest common supertype detection here
    assert(false);
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

void NodeExprUnary::assignUnresolvedTypes() {
  arg->assignUnresolvedTypes();
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

void NodeExprTuple::assignUnresolvedTypes() {
  for(auto i = elements.begin(); i != elements.end(); ++i) {
    (*i)->assignUnresolvedTypes();
  }
}

template<class T> static void alloc(Assembly &assembly, T size) {
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

void NodeExprTuple::compile(Assembly &assembly) {
  getType();
  assert(type);

  if(type->getTupleWidth() != 1) {
    alloc(assembly, type->getHeapSize());
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
  }
}

void NodeExprTuple::compileL(Assembly &assembly) {
  assert(false);
}

Type *NodeExprTuple::getType() {
  if(!type) {
    type = new TypeTuple();
    for(auto i = elements.begin(); i != elements.end(); ++i) {
      Type *et = (*i)->getType();
      if(dynamic_cast<TypeTuple *>(et)) {
        // TODO: flatten
        assert(false);
      } else {
        type->addElementType(et);
      }
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

    void *loopBegin(Assembly &assembly, Type *result) {
      assembly.add(push(rdx()));
      assembly.add(push(rcx()));
      assembly.add(push(rbx()));
      assembly.add(push(rdi()));
      assembly.add(move(rax(), rbx()));

      assembly.add(move(memory(-8, rbx()), rax()));
      // TODO: mask lower bits
      assembly.add(move(innerType->getSize(), rdi()));
      assembly.add(move(static_cast<uint64_t>(0), rdx()));
      assembly.add(div(rdi()));
      assembly.add(move(result->getSize(), rdi()));
      assembly.add(mul(rdi()));
      alloc(assembly, rax());
      assembly.add(move(rax(), rdi()));
      assembly.add(push(rdi()));

      assembly.add(move(rbx(), rdx()));
      LoopLabels *labels = new LoopLabels();
      labels->start = assembly.label();
      labels->end = assembly.label();

      assembly.add(labels->start);
      assembly.add(move(rbx(), rcx()));
      assembly.add(sub(rdx(), rcx()));
      assembly.add(move(memory(-8, rdx()), rax()));
      // TODO: mask lower 3 bits
      assembly.add(cmp(rax(), rcx()));
      assembly.add(jae(labels->end));

      assembly.add(move(memory(rbx()), rcx()));

      return labels;
    }

    void loopStep(Assembly &, Type *, void *) { }

    void loopStepSecondary(Assembly &, Type *, void *) {
      assert(false);
    }

    void loopEnd(Assembly &assembly, Type *result, void *data) {
      LoopLabels *labels = reinterpret_cast<LoopLabels *>(data);
      switch(result->getSize()) {
        case 8:
          assembly.add(move(rax(), memory(rdi())));
          break;
        default:
          assert(false);
      }

      assembly.add(add(innerType->getSize(), rbx()));
      assembly.add(add(innerType->getSize(), rdi()));
      assembly.add(jmp(labels->start));
      assembly.add(labels->end);

      assembly.add(pop(rdi()));
      assembly.add(move(rdi(), rax()));
      assembly.add(pop(rdi()));
      assembly.add(pop(rbx()));
      assembly.add(pop(rcx()));
      assembly.add(pop(rdx()));

      delete labels;
    }

    void compile(Assembly &) {
      // TODO: initialize arrays
      assert(false);
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

void NodeExprArray::assignUnresolvedTypes() {
  for(auto i = elements.begin(); i != elements.end(); ++i) {
    (*i)->assignUnresolvedTypes();
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
  Type *inner = Type::all;

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
    void assignUnresolvedTypes() { }
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

// 1. for each function of the overload set
// 2. from top to bottom search for tuples of right count
//    one-tuples and scalars are skipped (unless function has 1 argument)
//    lists are looped (but if 1 argument function, top level tuple matches)
// 3. for each argument of the function, search within the
//    appropriate element of the tuple, with the search algorithm
//    specified by rank of the argument
//    one-tuples and scalars are skipped (unless matching)
//    lists are looped (if traversed)
// 4. create unified (except for initial tuple scan) loops
//    start from top of types
//    4a. take the type of the leftmost argument
//    4b. all arguments with a compatible loopable type are looped
//        the result dimension is of the looped type (or their supertypes)
//    4c. all arguments with an incomptible type are not looped but
//        replicated along the dimension
//    4d. all arguments looped have their topmost type removed accordingly
// 5. (repeatedly) invoke the function until all elements have
//    been processed
// 6. if no function of the overload set matched, report error
void NodeExprApply::rewriteFunctionApplications(NodeExpr **parent) {
  argument->rewriteFunctionApplications(&argument);

  TypeFunctionSet *fts = dynamic_cast<TypeFunctionSet *>(function->getType());
  if(fts) {
    function = fts->get(argument->getType());
    if(!function) {
      compileError("no matching overload function found for " +
          fts->dump(0));
    }
  }

  TypeFunction *ft = dynamic_cast<TypeFunction *>(function->getType());
  if(!ft) compileError("cannot apply non-function " + function->dump(0));

  Type *at = argument->getType();

  if(ft->getArgumentCount() != 1) {
    while(at->getTupleWidth() == 1) {
      if(TypeTuple *att = dynamic_cast<TypeTuple *>(at)) {
        at = att->getElementType(0);
      } else if(TypeLoopable *atl = dynamic_cast<TypeLoopable *>(at)) {
        *parent = new NodeExprLoop(argument, -1,
            new NodeExprApply(function, new RCX(atl->getInnerType())));
        (*parent)->rewriteFunctionApplications(parent);
        return;
      } else {
        compileError("overload resolution fucked up, "
            "autolooping is deeply sorry (1): " + function->dump(0));
      }
    }
  }

  if(at->getTupleWidth() != ft->getArgumentCount()) {
    compileError("overload resolution fucked up, "
        "autolooping is deeply sorry (2): " + function->dump(0));
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
      // apply function first
      assert(false);
    } else if(auto argfst = dynamic_cast<TypeFunctionSet *>(argt) && !dynamic_cast<TypeFunctionSet *>(fargt)) {
      if(auto fargft = dynamic_cast<TypeFunction *>(fargt)) {
        // resolve function set and use resulting function
        assert(false);
      } else {
        // create new function set
        std::string unique = createUniqueIdentifier();

        NodeExprList *argumentList = new NodeExprList();
        if(auto argtuple = dynamic_cast<NodeExprTuple *>(argument)) {
          for(int j = 0; j < ft->getArgumentCount(); ++j) {
            if(j != i) {
              argumentList->add(argtuple->getElements().at(j));
            } else {
              argumentList->add(new NodeExprApply(argtuple->getElements().at(j), new NodeIdentifier(unique)));
            }
          }
        } else {
          assert(i == 0);

          argumentList->add(argument);
        }

        *parent = new NodeExprUntypedLambda(
            unique,
            new NodeStatementExpr(
              new NodeExprApply(function,
                new NodeExprTuple(argumentList))),
            syms);

        (*parent)->rewriteFunctionApplications(parent);
        // TODO: correctly handle co-looping
        return;
      }
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
        } else if(TypeLoopable *atl = dynamic_cast<TypeLoopable *>(argt)) {
          argt = atl->getInnerType();
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
        if(at->getTupleWidth() > 1) {
          TypeTuple *origTuple = dynamic_cast<TypeTuple *>(at);
          assert(origTuple);

          TypeTuple *innerType = new TypeTuple();
          for(int j = 0; j < at->getTupleWidth(); ++j) {
            if(i == j) {
              innerType->addElementType(atl->getInnerType());
            } else {
              innerType->addElementType(origTuple->getElementType(j));
            }
          }

          *parent = new NodeExprLoop(argument, i,
              new NodeExprApply(function, new RCX(innerType)));
        } else {
          *parent = new NodeExprLoop(argument, -1,
              new NodeExprApply(function, new RCX(atl->getInnerType())));
        }
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

void NodeExprApply::assignUnresolvedTypes() {
  // TODO: resolve TypeFunctionSets here
}

void NodeExprApply::compile(Assembly &assembly) {
  TypeFunction *ft = dynamic_cast<TypeFunction *>(function->getType());
  assert(ft);

  Type *at = argument->getType();

  NodeExpr *arg = argument;

  if(arg->getType()->getTupleWidth() > 1) {
    Register64 *abiArgs[] = { rdi(), rsi(), rdx(), rcx(), r8(), r9() };
    TypeTuple *argt = dynamic_cast<TypeTuple *>(arg->getType());
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
}

void NodeExprApply::compileL(Assembly &assembly) {
  assert(false);
}

Type *NodeExprApply::getType() {
  TypeFunction *ft = dynamic_cast<TypeFunction *>(function->getType());
  assert(ft);

  return ft->getReturnType();
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

void NodeExprProjection::assignUnresolvedTypes() {
  // nothing to do
}

void NodeExprProjection::compile(Assembly &assembly) {
  assert(false);
}

void NodeExprProjection::compileL(Assembly &assembly) {
  assert(false);
}

void *projection0_0(void *v) { return v; }
void *projection1_0(void *, void *v) { return v; }
void *projection2_0(void *, void *, void *v) { return v; }
void *projection3_0(void *, void *, void *, void *v) { return v; }
void *projection4_0(void *, void *, void *, void *, void *v) { return v; }
void *projection5_0(void *, void *, void *, void *, void *, void *v) { return v; }

Type *NodeExprProjection::getType() {
  return new (class _: public TypeFunctionSet {
    public:
      _(int pos, int level): pos(pos), level(level) { }

      unsigned int getSize() { return 8; }
      NodeExprFunction *get(Type *argumentType) {
        if(level) {
          // TODO: think about this
          assert(false);
        } else {
          while(TypeArray *tarr = dynamic_cast<TypeArray *>(argumentType)) {
            argumentType = tarr->getInnerType();
          }

          if(argumentType->getTupleWidth() == 1) {
            if(pos) return 0;

            TypeFunction *type =
              (new TypeFunction())
              ->setReturnType(argumentType)
              ->addArgument(argumentType, 1);

            return new (class _: public NodeExprFunction {
              public:
                _(int pos, Type *type): pos(pos), type(type) { }

                void rewriteDeclarations(SymbolTable *, NodeExpr **) { assert(false); }
                void resolveSymbols(SymbolTable *) { assert(false); }
                void rewriteFunctionApplications(NodeExpr **) { assert(false); }
                void assignUnresolvedTypes() { assert(false); }
                void compile(Assembly &assembly) {
                  assembly.add(move(
                      reinterpret_cast<uint64_t>(&projection0_0), rax()));
                }
                void compileL(Assembly &) { compileError("built-in function is not an l-value"); }
                Type *getType() { return type; }

              private:
                int pos;
                Type *type;
            }) (pos, type);
          } else {
            TypeTuple *tt = dynamic_cast<TypeTuple *>(argumentType);
            assert(tt);

            if(pos >= tt->getTupleWidth()) return 0;

            TypeFunction *type = (new TypeFunction())
              ->setReturnType(tt->getElementType(pos));
            for(int i = 0; i < tt->getTupleWidth(); ++i) {
              type->addArgument(tt->getElementType(i), 1);
            }

            return new (class _: public NodeExprFunction {
              public:
                _(int pos, Type *type): pos(pos), type(type) { }

                void rewriteDeclarations(SymbolTable *, NodeExpr **) { assert(false); }
                void resolveSymbols(SymbolTable *) { assert(false); }
                void rewriteFunctionApplications(NodeExpr **) { assert(false); }
                void assignUnresolvedTypes() { assert(false); }
                void compile(Assembly &assembly) {
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
                void compileL(Assembly &) { compileError("built-in function is not an l-value"); }
                Type *getType() { return type; }

              private:
                int pos;
                Type *type;
            }) (pos, type);
          }
        }
      }

      void compile(Assembly &) { assert(false); }

    private:
      int pos;
      int level;
  }) (pos, level);
}

void NodeExprLoop::rewriteDeclarations(SymbolTable *, NodeExpr **) {
  // these nodes should only be created after symbol resolution has
  // already taken place
  assert(false);
}

void NodeExprLoop::resolveSymbols(SymbolTable *) {
  // these nodes should only be created after symbol resolution has
  // already taken place
  assert(false);
}

void NodeExprLoop::rewriteFunctionApplications(NodeExpr **parent) {
  container->rewriteFunctionApplications(&container);
  expr->rewriteFunctionApplications(&expr);
}

void NodeExprLoop::assignUnresolvedTypes() {
  container->assignUnresolvedTypes();
  expr->assignUnresolvedTypes();
}

void NodeExprLoop::compile(Assembly &assembly) {
  if(tupleIndex == -1) {
    TypeLoopable *ct = dynamic_cast<TypeLoopable *>(container->getType());
    assert(ct);
    Type *result = expr->getType();

    container->compile(assembly);
    void *data = ct->loopBegin(assembly, result);
    ct->loopStep(assembly, result, data);
    expr->compile(assembly);
    ct->loopEnd(assembly, result, data);
  } else {
    TypeTuple *tt = dynamic_cast<TypeTuple *>(container->getType());
    assert(tt);
    TypeLoopable *ct = dynamic_cast<TypeLoopable *>(tt->getElementType(tupleIndex));
    assert(ct);
    Type *result = expr->getType();

    TypeTuple *nt = new TypeTuple();
    for(int i = 0; i < tt->getTupleWidth(); ++i) {
      if(i == tupleIndex) {
        nt->addElementType(ct->getInnerType());
      } else {
        nt->addElementType(tt->getElementType(i));
      }
    }

    container->compile(assembly);
    assembly.add(push(rsi()));
    assembly.add(move(rax(), rsi()));
    assembly.add(move(memory(tt->getElementOffset(tupleIndex), rsi()), rax()));
    void *data = ct->loopBegin(assembly, result);
    ct->loopStep(assembly, result, data);

    alloc(assembly, nt->getHeapSize());

    for(int i = 0; i < tt->getTupleWidth(); ++i) {
      if(i == tupleIndex) {
        switch(tt->getElementType(i)->getSize()) {
          case 8:
            assembly.add(move(rcx(), memory(nt->getElementOffset(i), rax())));
            break;
          default:
            assert(false);
        }
      } else {
        switch(tt->getElementType(i)->getSize()) {
          case 8:
            assembly.add(push(rdx()));
            assembly.add(move(memory(tt->getElementOffset(i), rsi()), rdx()));
            assembly.add(move(rdx(), memory(nt->getElementOffset(i), rax())));
            assembly.add(pop(rdx()));
            break;
          default:
            assert(false);
        }
      }
    }
    
    assembly.add(move(rax(), rcx()));
    expr->compile(assembly);
    ct->loopEnd(assembly, result, data);

    assembly.add(pop(rsi()));
  }
}

void NodeExprLoop::compileL(Assembly &assembly) {
  assert(false);
}

Type *NodeExprLoop::getType() {
  if(!type) {
    if(tupleIndex == -1) {
      TypeLoopable *tl = dynamic_cast<TypeLoopable *>(container->getType());
      assert(tl);

      type = tl->generate(expr->getType());
    } else {
      TypeTuple *tt = dynamic_cast<TypeTuple *>(container->getType());
      assert(tt);
      TypeLoopable *tl =
        dynamic_cast<TypeLoopable *>(tt->getElementType(tupleIndex));
      assert(tl);

      type = tl->generate(expr->getType());
    }
  }

  return type;
}

std::string NodeExprLoop::dump(int i) {
  std::ostringstream str;
  str << indent(i) <<
    "NodeExprLoop (index: " << tupleIndex << ")\n"
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

static int32_t addition_int32_t(int32_t a, int32_t b) {
  return a + b;
}

void NodeExprAdd::rewriteFunctionApplications(NodeExpr **parent) {
  *parent = new NodeExprApply(
      new (class _: public NodeExpr {
        public:
          _(): type((new TypeFunction())
            ->setReturnType(Type::sint32)
            ->addArgument(Type::sint32, 1)
            ->addArgument(Type::sint32, 1)) { }
          void rewriteDeclarations(SymbolTable *, NodeExpr **) { assert(false); }
          void resolveSymbols(SymbolTable *) { }
          void rewriteFunctionApplications(NodeExpr **) { }
          void assignUnresolvedTypes() { }
          void compile(Assembly &assembly) {
            assembly.add(move(reinterpret_cast<uint64_t>(&addition_int32_t), rax()));
          }
          void compileL(Assembly &assembly) { compileError("no writing to predefined functions"); }
          Type *getType() { return type; }
          std::string dump(int i) { return indent(i) + "α + β"; }

        private:
          Type *type;
      }) (),
      new NodeExprTuple(lhs, rhs));
  
  (*parent)->resolveSymbols(syms);
  (*parent)->rewriteFunctionApplications(parent);
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

void NodeExprDeclaration::assignUnresolvedTypes() {
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

void NodeExprAssign::rewriteFunctionApplications(NodeExpr **parent) {
  rhs->rewriteFunctionApplications(&rhs);
  lhs->rewriteFunctionApplications(&lhs);

  if(auto rhst = dynamic_cast<TypeFunctionSet *>(rhs->getType())) {
    if(auto lhst = lhs->getType()) {
      NodeExpr *newRight = rhst->get(lhst);
      if(newRight) {
        rhs = newRight;
        rhs->rewriteFunctionApplications(&rhs);
      } else {
        // create new function set
        std::string unique = createUniqueIdentifier();

        *parent = new NodeExprUntypedLambda(
            unique,
            new NodeStatementExpr(
              new NodeExprApply(rhs, new NodeIdentifier(unique))),
            syms);

        (*parent)->rewriteFunctionApplications(parent);
      }
    }
  }
}

void NodeExprAssign::assignUnresolvedTypes() {
  lhs->assignUnresolvedTypes();
  rhs->assignUnresolvedTypes();
}

void NodeExprAssign::compile(Assembly &assembly) {
  // TODO: do the function application rewriting
  if(*lhs->getType() != *rhs->getType()) {
    std::cerr << "LHS: \n" << lhs->getType()->dump(0) << std::endl;
    std::cerr << "RHS: \n" << rhs->getType()->dump(0) << std::endl;
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

  if(argt->getTupleWidth() == 1) {
    func->addArgument(argt, 1);
  } else {
    TypeTuple *args = dynamic_cast<TypeTuple *>(argt);
    assert(args);

    for(int i = 0; i < args->getTupleWidth(); ++i) {
      func->addArgument(args->getElementType(i), 1);
    }
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

void NodeTypedLambda::assignUnresolvedTypes() {
  body->assignUnresolvedTypes();
}

void NodeTypedLambda::compile(Assembly &assembly) {
  Assembly code;

  if(localSymbols->getHeapVariableSize()) {
    // get data area address
    code.add(pop(r8()));
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

void NodeExprUnaryMinus::compile(Assembly &assembly) {
  arg->compile(assembly);
  assembly.add(neg(rax()));
}

NodeExprUntypedLambda::NodeExprUntypedLambda(const std::string &argumentName, NodeStatement *body, SymbolTable *parentSymbols)
  : argumentName(argumentName), body(body), syms(parentSymbols) {
  assert(body);
  assert(parentSymbols);
}

void NodeExprUntypedLambda::rewriteDeclarations(SymbolTable *symbols, NodeExpr **parent) {
}

void NodeExprUntypedLambda::resolveSymbols(SymbolTable *st) {
  assert(st);

  syms = st;
}

void NodeExprUntypedLambda::rewriteFunctionApplications(NodeExpr **) {
  // no rewrites here, as type of the argument tuple variable is still unknown
}

void NodeExprUntypedLambda::assignUnresolvedTypes() {
  // this should not be called - these nodes need to be replaced by something uptree setting the type
  // afterwards the new node will have to get assignUnresolvedTypes called
  assert(false);
}

void NodeExprUntypedLambda::compile(Assembly &) {
  assert(false);
}

void NodeExprUntypedLambda::compileL(Assembly &) {
  assert(false);
}

Type *NodeExprUntypedLambda::getType() {
  return new TypeNodeBackedFunctionSet(this);
}

NodeExprFunction *NodeExprUntypedLambda::assignType(Type *type) {
  if(auto ft = dynamic_cast<TypeFunction *>(type)) {
    if(ft->getArgumentCount() == 1) {
      NodeExprFunction *replacement = new NodeTypedLambda(
          new NodeExprTuple(new NodeExprDeclaration(ft->getArgumentType(0), new NodeIdentifier(argumentName))),
          ft->getReturnType(),
          body);

      replacement->rewriteDeclarations(syms, static_cast<NodeExpr **>(0));
      replacement->resolveSymbols(syms);
      return replacement;
    } else {
      assert(false);
    }
  }

  return 0;
}

unsigned long TreeNode::creations = 0;
