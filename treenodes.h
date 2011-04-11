#ifndef TREENODES_H
#define TREENODES_H

#include "noreturn.h"

#include <string>
#include <algorithm>
#include <typeinfo>
#include <assert.h>

class Assembly;
class Main;
class Type;
class TypeFunction;
class TypeTuple;
class TypeDomained;
class SymbolTable;
class SymbolTableClosure;
class Symbol;
class NodeExpr;

class TreeNode {
  public:
    TreeNode() { ++creations; }
    virtual ~TreeNode() { }
    virtual std::string dump(int indent);

    static unsigned long creations;

  protected:
    void NORET compileError(const std::string &);

    std::string indent(int indent);
};

class NodeStatement: public TreeNode {
  public:
    virtual void rewriteDeclarations(SymbolTable *) = 0;
    virtual void resolveSymbols(SymbolTable *) = 0;
    virtual void rewriteFunctionApplications() = 0;
    virtual bool assignUnresolvedTypes(Type *) = 0;
    virtual void compile(Assembly &) = 0;
};

class NodeExpr: public TreeNode {
  public:
    NodeExpr() { }

    virtual void rewriteDeclarations(SymbolTable *, NodeExpr **parent) = 0;
    virtual void resolveSymbols(SymbolTable *) = 0;
    virtual void rewriteFunctionApplications(NodeExpr **parent) = 0;
    virtual bool assignUnresolvedTypes(Type *) = 0;
    virtual void compile(Assembly &) = 0;
    virtual void compileL(Assembly &) = 0;
    virtual Type *getType() = 0;
};

class NodeIdentifier: public NodeExpr {
  public:
    NodeIdentifier(const std::string &id): id(id) { }
    virtual const std::string &get() const { return id; }
    void rewriteDeclarations(SymbolTable *, NodeExpr **);
    void resolveSymbols(SymbolTable *);
    void rewriteFunctionApplications(NodeExpr **) { }
    bool assignUnresolvedTypes(Type *t);
    void compile(Assembly &);
    void compileL(Assembly &);
    Type *getType();
    std::string dump(int);

  private:
    const std::string id;
    Symbol *symbol;
};

class NodeComment: public TreeNode {
  public:
    NodeComment(const std::string &value): value(value) { }
    virtual const std::string &get() { return value; }
    
  private:
    std::string value;
};

class NodeString: public NodeExpr {
  public:
    NodeString(const std::string &value): value(value) { }
    virtual const std::string &get() { return value; }
    void rewriteDeclarations(SymbolTable *, NodeExpr **) { }
    void resolveSymbols(SymbolTable *);
    void rewriteFunctionApplications(NodeExpr **) { }
    bool assignUnresolvedTypes(Type *);
    void compile(Assembly &);
    void compileL(Assembly &);
    Type *getType();

  private:
    const std::string &value;
};

class NodeInteger: public NodeExpr {
  public:
    NodeInteger(const unsigned long long &val): val(val) { }
    void rewriteDeclarations(SymbolTable *, NodeExpr **) { }
    void resolveSymbols(SymbolTable *);
    void rewriteFunctionApplications(NodeExpr **) { }
    bool assignUnresolvedTypes(Type *);
    void compile(Assembly &);
    void compileL(Assembly &);
    Type *getType();

  private:
    unsigned long long val;
};

class NodeExprList: public TreeNode {
  public:
    NodeExprList() { }

    NodeExprList(NodeExpr *expr) {
      assert(expr);

      exprs.push_back(expr);
    }

    NodeExprList(NodeExpr *expr, NodeExprList *xs) {
      assert(expr);
      assert(xs);

      exprs.push_back(expr);
      std::copy(xs->exprs.begin(), xs->exprs.end(), back_inserter(exprs));
    }

    void add(NodeExpr *expr) {
      assert(expr);

      exprs.push_back(expr);
    }

  private:
    std::vector<NodeExpr *> exprs;

    friend class NodeExprTuple;
    friend class NodeExprArray;
};

class NodeExprTuple: public NodeExpr {
  public:
    NodeExprTuple(NodeExprList *);
    NodeExprTuple(NodeExpr *);
    NodeExprTuple(NodeExpr *, NodeExpr *);

    void rewriteDeclarations(SymbolTable *, NodeExpr **);
    void resolveSymbols(SymbolTable *);
    void rewriteFunctionApplications(NodeExpr **);
    bool assignUnresolvedTypes(Type *);
    void compile(Assembly &);
    void compileL(Assembly &);
    Type *getType();
    std::string dump(int);

    const std::vector<NodeExpr *> &getElements() const { return elements; }

  private:
    std::vector<NodeExpr *> elements;

    Type *type;
};

class NodeExprArray: public NodeExpr {
  public:
    NodeExprArray(NodeExprList *list) {
      assert(list);

      elements = list->exprs;
    }

    void rewriteDeclarations(SymbolTable *, NodeExpr **);
    void resolveSymbols(SymbolTable *);
    void rewriteFunctionApplications(NodeExpr **);
    bool assignUnresolvedTypes(Type *);
    void compile(Assembly &);
    void compileL(Assembly &);
    Type *getType();
    std::string dump(int);

  private:
    std::vector<NodeExpr *> elements;
};

class NodeExprApply: public NodeExpr {
  public:
    NodeExprApply(NodeExpr *function, NodeExpr *argument);

    void rewriteDeclarations(SymbolTable *, NodeExpr **);
    void resolveSymbols(SymbolTable *);
    void rewriteFunctionApplications(NodeExpr **);
    bool assignUnresolvedTypes(Type *);
    void compile(Assembly &);
    void compileL(Assembly &);
    Type *getType() { return type; }
    std::string dump(int);

  private:
    NodeExpr *function;
    NodeExpr *argument;

    SymbolTable *syms;
    Type *type;

    void abstractTypeDomainedFull(NodeExpr **, TypeDomained *);
    void abstractTypeDomainedPositioned(NodeExpr **, const std::vector<int> &);
};

class NodeStatementExpr: public NodeStatement {
  public:
    NodeStatementExpr(NodeExpr *expr): expr(expr) { }

    void rewriteDeclarations(SymbolTable *);
    void resolveSymbols(SymbolTable *);
    void rewriteFunctionApplications();
    bool assignUnresolvedTypes(Type *);
    void compile(Assembly &);
    std::string dump(int);

  private:
    NodeExpr *expr;
};

class NodeStatementWhile: public NodeStatement {
  public:
    NodeStatementWhile(NodeExpr *condition, NodeStatement *body)
      : condition(condition), body(body) {
      assert(condition);
      assert(body);
    }

    void rewriteDeclarations(SymbolTable *);
    void resolveSymbols(SymbolTable *);
    void rewriteFunctionApplications();
    bool assignUnresolvedTypes(Type *);
    void compile(Assembly &);

  private:
    NodeExpr *condition;
    NodeStatement *body;
};

class NodeStatementGoto: public NodeStatement {
  public:
    NodeStatementGoto(NodeExpr *target)
      : target(target) {
      assert(target);
    }

    // TODO: no new declarations while gotoing, for the time being...
    void rewriteDeclarations(SymbolTable *) { }
    void resolveSymbols(SymbolTable *);
    void rewriteFunctionApplications();
    bool assignUnresolvedTypes(Type *);
    void compile(Assembly &);

  private:
    NodeExpr *target;
};

class NodeStatementLabel: public NodeStatement {
  public:
    NodeStatementLabel(NodeIdentifier *name)
      : name(name) {
      assert(name);
    }

    // TODO: no new declarations within labels for the time being...
    void rewriteDeclarations(SymbolTable *) { }
    void resolveSymbols(SymbolTable *);
    void rewriteFunctionApplications();
    bool assignUnresolvedTypes(Type *);
    void compile(Assembly &);

  private:
    NodeIdentifier *name;
};

class NodeStatementRule: public NodeStatement {
  public:
    NodeStatementRule(NodeIdentifier *lhs, NodeExprTuple *rhs,
        NodeStatement *mapping)
      : lhs(lhs), rhs(rhs), mapping(mapping) { }

    void rewriteDeclarations(SymbolTable *);
    void resolveSymbols(SymbolTable *);
    void rewriteFunctionApplications();
    bool assignUnresolvedTypes(Type *);
    void compile(Assembly &);
    void compileL(Assembly &);

  private:
    NodeIdentifier *lhs;
    NodeExprTuple *rhs;
    NodeStatement *mapping;
};

class NodeStatementList: public TreeNode {
  public:
    NodeStatementList() { }

    NodeStatementList(NodeStatement *stmt) {
      assert(stmt);

      statements.push_back(stmt);
    }

    NodeStatementList(NodeStatement *stmt, NodeStatementList *list) {
      assert(stmt);
      assert(list);

      statements.push_back(stmt);
      std::copy(list->statements.begin(),
          list->statements.end(), back_inserter(statements));
    }

    const std::vector<NodeStatement *> &getStatements() const {
      return statements;
    }

  private:
    std::vector<NodeStatement *> statements;
};

class NodeStatementBlock: public NodeStatement {
  public:
    NodeStatementBlock() { }

    NodeStatementBlock(NodeStatementList *stmts) {
      statements = stmts->getStatements();
    }

    void rewriteDeclarations(SymbolTable *);
    void resolveSymbols(SymbolTable *);
    void rewriteFunctionApplications();
    bool assignUnresolvedTypes(Type *);
    void compile(Assembly &);

  private:
    std::vector<NodeStatement *> statements;
};

class NodeExprFunction: public NodeExpr {
  public:
    NodeExprFunction() { }
};

class NodeExprProjection: public NodeExprFunction {
  public:
    NodeExprProjection(int pos, int level): pos(pos), level(level), type(0) { }
    NodeExprProjection(int pos, int level, TypeFunction *type): pos(pos), level(level), type(type) { }
    void rewriteDeclarations(SymbolTable *, NodeExpr **) { }
    void resolveSymbols(SymbolTable *);
    void rewriteFunctionApplications(NodeExpr **);
    bool assignUnresolvedTypes(Type *);
    void compile(Assembly &);
    void compileL(Assembly &);
    Type *getType();
    std::string dump(int);

  private:
    size_t pos;
    size_t level;

    TypeFunction *type;
};

class NodeExprUnary: public NodeExpr {
  public:
    NodeExprUnary(NodeExpr *arg): arg(arg) {
      assert(arg);
    }

    void rewriteDeclarations(SymbolTable *, NodeExpr **);
    void resolveSymbols(SymbolTable *);
    void rewriteFunctionApplications(NodeExpr **);
    bool assignUnresolvedTypes(Type *);
    void compile(Assembly &);
    void compileL(Assembly &);
    Type *getType();
    std::string dump(int);
    
  protected:
    NodeExpr *arg;

    virtual std::string op() = 0;
};

class NodeExprBinary: public NodeExpr {
  public:
    NodeExprBinary(NodeExpr *lhs, NodeExpr *rhs): lhs(lhs), rhs(rhs) {
      assert(lhs);
      assert(rhs);
    }

    void resolveSymbols(SymbolTable *) { assert(false); }
    void rewriteFunctionApplications(NodeExpr **) { assert(false); }
    bool assignUnresolvedTypes(Type *) { assert(false); }
    void compile(Assembly &) { assert(false); }
    void compileL(Assembly &) { assert(false); }
    Type *getType() { assert(false); }
    std::string dump(int);
    
  protected:
    NodeExpr *lhs;
    NodeExpr *rhs;

    virtual std::string op() = 0;
};

class NodeExprAssign: public NodeExprBinary {
  public:
    NodeExprAssign(NodeExpr *lhs, NodeExpr *rhs): NodeExprBinary(lhs, rhs), type(0), syms(0) { }
    std::string op() { return "="; }

    void rewriteDeclarations(SymbolTable *, NodeExpr **);
    void resolveSymbols(SymbolTable *);
    void rewriteFunctionApplications(NodeExpr **);
    bool assignUnresolvedTypes(Type *);
    void compile(Assembly &);
    void compileL(Assembly &);

  private:
    Type *type;
    SymbolTable *syms;
};

class NodeExprBinarySimple: public NodeExprBinary {
  public:
    NodeExprBinarySimple(NodeExpr *lhs, NodeExpr *rhs): NodeExprBinary(lhs, rhs) { }

    void rewriteDeclarations(SymbolTable *, NodeExpr **);

  protected:
    virtual NodeExpr *implementation();

    class NodeExprConstantFunction: public NodeExpr {
      public:
        NodeExprConstantFunction(Type *type): type(type) { }

        void rewriteDeclarations(SymbolTable *, NodeExpr **) { }
        void resolveSymbols(SymbolTable *) { }
        void rewriteFunctionApplications(NodeExpr **) { }
        bool assignUnresolvedTypes(Type *);
        void compileL(Assembly &) { compileError("no writing to predefined functions"); }
        Type *getType() { return type; }
        std::string dump(int i) { return indent(i) + "constant function"; }

      private:
        Type *type;
    };
};

class NodeExprLogicalAnd: public NodeExprBinarySimple {
  public:
    NodeExprLogicalAnd(NodeExpr *lhs, NodeExpr *rhs): NodeExprBinarySimple(lhs, rhs) { }
    std::string op() { return "&&"; }
};

class NodeExprLogicalOr: public NodeExprBinarySimple {
  public:
    NodeExprLogicalOr(NodeExpr *lhs, NodeExpr *rhs): NodeExprBinarySimple(lhs, rhs) { }
    std::string op() { return "||"; }
};

class NodeExprLogicalXor: public NodeExprBinarySimple {
  public:
    NodeExprLogicalXor(NodeExpr *lhs, NodeExpr *rhs): NodeExprBinarySimple(lhs, rhs) { }
    std::string op() { return "^^"; }
};

class NodeExprBitwiseAnd: public NodeExprBinarySimple {
  public:
    NodeExprBitwiseAnd(NodeExpr *lhs, NodeExpr *rhs): NodeExprBinarySimple(lhs, rhs) { }
    std::string op() { return "&"; }
};

class NodeExprBitwiseOr: public NodeExprBinarySimple {
  public:
    NodeExprBitwiseOr(NodeExpr *lhs, NodeExpr *rhs): NodeExprBinarySimple(lhs, rhs) { }
    std::string op() { return "|"; }
};

class NodeExprBitwiseXor: public NodeExprBinarySimple {
  public:
    NodeExprBitwiseXor(NodeExpr *lhs, NodeExpr *rhs): NodeExprBinarySimple(lhs, rhs) { }
    std::string op() { return "^"; }
};

class NodeExprEqual: public NodeExprBinarySimple {
  public:
    NodeExprEqual(NodeExpr *lhs, NodeExpr *rhs): NodeExprBinarySimple(lhs, rhs) { }
    std::string op() { return "=="; }
};

class NodeExprLogicalNot: public NodeExprUnary {
  public:
    NodeExprLogicalNot(NodeExpr *arg): NodeExprUnary(arg) { }
    std::string op() { return "!!"; }
};

class NodeExprUnaryMinus: public NodeExprUnary {
  public:
    NodeExprUnaryMinus(NodeExpr *arg): NodeExprUnary(arg) { }
    std::string op() { return "-"; }

    virtual void compile(Assembly &);
};

class NodeExprLessThan: public NodeExprBinarySimple {
  public:
    NodeExprLessThan(NodeExpr *lhs, NodeExpr *rhs): NodeExprBinarySimple(lhs, rhs) { }
    std::string op() { return "<"; }
};

class NodeExprLessThanEqual: public NodeExprBinarySimple {
  public:
    NodeExprLessThanEqual(NodeExpr *lhs, NodeExpr *rhs): NodeExprBinarySimple(lhs, rhs) { }
    std::string op() { return "<="; }
};

class NodeExprShiftLeft: public NodeExprBinarySimple {
  public:
    NodeExprShiftLeft(NodeExpr *lhs, NodeExpr *rhs): NodeExprBinarySimple(lhs, rhs) { }
    std::string op() { return "<<"; }
};

class NodeExprShiftRight: public NodeExprBinarySimple {
  public:
    NodeExprShiftRight(NodeExpr *lhs, NodeExpr *rhs): NodeExprBinarySimple(lhs, rhs) { }
    std::string op() { return ">>"; }
};

class NodeExprAdd: public NodeExprBinarySimple {
  public:
    NodeExprAdd(NodeExpr *lhs, NodeExpr *rhs): NodeExprBinarySimple(lhs, rhs) { }
    std::string op() { return "+"; }

    NodeExpr *implementation();
};

class NodeExprSub: public NodeExprBinarySimple {
  public:
    NodeExprSub(NodeExpr *lhs, NodeExpr *rhs): NodeExprBinarySimple(lhs, rhs) { }
    std::string op() { return "-"; }
};

class NodeExprMul: public NodeExprBinarySimple {
  public:
    NodeExprMul(NodeExpr *lhs, NodeExpr *rhs): NodeExprBinarySimple(lhs, rhs) { }
    std::string op() { return "*"; }
};

class NodeExprDiv: public NodeExprBinarySimple {
  public:
    NodeExprDiv(NodeExpr *lhs, NodeExpr *rhs): NodeExprBinarySimple(lhs, rhs) { }
    std::string op() { return "/"; }
};

class NodeExprMod: public NodeExprBinarySimple {
  public:
    NodeExprMod(NodeExpr *lhs, NodeExpr *rhs): NodeExprBinarySimple(lhs, rhs) { }
    std::string op() { return "%"; }
};

class NodeExprLoop: public NodeExpr {
  public:
    NodeExprLoop(NodeExpr *argument, const std::string &argName, NodeExpr *domain, const std::string &keyName, NodeExpr *expr)
      : argument(argument), domain(domain), argName(argName), keyName(keyName),
        argId(0), keyId(0), targetId(0), expr(expr), type(0) { }

    void rewriteDeclarations(SymbolTable *, NodeExpr **);
    void resolveSymbols(SymbolTable *);
    void rewriteFunctionApplications(NodeExpr **);
    bool assignUnresolvedTypes(Type *);
    void compile(Assembly &);
    void compileL(Assembly &);
    Type *getType();
    std::string dump(int i);

  private:
    NodeExpr *argument;
    NodeExpr *domain;
    std::string argName;
    std::string keyName;
    std::string targetName;
    NodeIdentifier *argId;
    NodeIdentifier *keyId;
    NodeIdentifier *targetId;
    NodeExpr *expr;
    Type *type;
};

class NodeExprDeclaration: public NodeExpr {
  public:
    NodeExprDeclaration(Type *type, NodeIdentifier *name)
      : type(type), name(name) { }

    void rewriteDeclarations(SymbolTable *, NodeExpr **);
    void resolveSymbols(SymbolTable *);
    void rewriteFunctionApplications(NodeExpr **);
    bool assignUnresolvedTypes(Type *);
    void compile(Assembly &);
    void compileL(Assembly &);
    Type *getType() { return type; }
    std::string dump(int i);

    const NodeIdentifier *getName() const { return name; }

  private:
    Type *type;
    NodeIdentifier *name;
};

class NodeTypeFunction: public NodeExpr {
  public:
    NodeTypeFunction(NodeExpr *arg, NodeExpr *ret): arg(arg), ret(ret) {
      assert(arg);
      assert(ret);
    }

    void rewriteDeclarations(SymbolTable *, NodeExpr **);
    void resolveSymbols(SymbolTable *) { assert(false); }
    void rewriteFunctionApplications(NodeExpr **) { assert(false); }
    bool assignUnresolvedTypes(Type *) { assert(false); }
    void compile(Assembly &) { assert(false); }
    void compileL(Assembly &) { assert(false); }
    Type *getType() { assert(false); }

  private:
    NodeExpr *arg;
    NodeExpr *ret;
};

// TODO: rename this to NodeExprTypedLambda
class NodeTypedLambda: public NodeExprFunction {
  public:
    NodeTypedLambda(NodeExprTuple *args, NodeExpr *ret, NodeStatement *body);

    void rewriteDeclarations(SymbolTable *, NodeExpr **);
    void resolveSymbols(SymbolTable *);
    void rewriteFunctionApplications(NodeExpr **);
    bool assignUnresolvedTypes(Type *);
    void compile(Assembly &);
    void compileL(Assembly &);
    Type *getType();
    std::string dump(int i);

  private:
    NodeExprTuple *args;
    NodeExpr *ret;
    NodeStatement *body;
    SymbolTableClosure *localSymbols;

    TypeFunction *type;
};

#endif
