#ifndef TREENODES_H
#define TREENODES_H

#include <string>
#include <algorithm>
#include <typeinfo>
#include <assert.h>

class Assembly;
class Main;
class Type;
class TypeFunction;
class TypeTuple;
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
    void compileError(const std::string &);

    std::string indent(int indent);
};

class NodeStatement: public TreeNode {
  public:
    virtual void rewriteDeclarations(SymbolTable *) = 0;
    virtual void resolveSymbols(SymbolTable *) = 0;
    virtual void rewriteFunctionApplications() = 0;
    virtual void assignUnresolvedTypes(Type *) = 0;
    virtual void compile(Assembly &) = 0;
};

class NodeExpr: public TreeNode {
  public:
    NodeExpr() { }

    virtual void rewriteDeclarations(SymbolTable *, NodeExpr **parent) = 0;
    virtual void resolveSymbols(SymbolTable *) = 0;
    virtual void rewriteFunctionApplications(NodeExpr **parent) = 0;
    virtual void assignUnresolvedTypes(Type *) = 0;
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
    void assignUnresolvedTypes(Type *) { }
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
    void assignUnresolvedTypes(Type *) { }
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
    void assignUnresolvedTypes(Type *) { }
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
    void assignUnresolvedTypes(Type *);
    void compile(Assembly &);
    void compileL(Assembly &);
    Type *getType();
    std::string dump(int);

    const std::vector<NodeExpr *> &getElements() const { return elements; }

  private:
    std::vector<NodeExpr *> elements;

    TypeTuple *type;
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
    void assignUnresolvedTypes(Type *);
    void compile(Assembly &);
    void compileL(Assembly &);
    Type *getType();
    std::string dump(int);

  private:
    std::vector<NodeExpr *> elements;
};

class NodeExprApply: public NodeExpr {
  public:
    NodeExprApply(NodeExpr *function, NodeExpr *argument)
      : function(function), argument(argument), syms(0) {
      assert(function);
      assert(argument);
    }

    void rewriteDeclarations(SymbolTable *, NodeExpr **);
    void resolveSymbols(SymbolTable *);
    void rewriteFunctionApplications(NodeExpr **);
    void assignUnresolvedTypes(Type *);
    void compile(Assembly &);
    void compileL(Assembly &);
    Type *getType();
    std::string dump(int);

  private:
    NodeExpr *function;
    NodeExpr *argument;

    SymbolTable *syms;
};

class NodeStatementExpr: public NodeStatement {
  public:
    NodeStatementExpr(NodeExpr *expr): expr(expr) { }

    void rewriteDeclarations(SymbolTable *);
    void resolveSymbols(SymbolTable *);
    void rewriteFunctionApplications();
    void assignUnresolvedTypes(Type *);
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
    void assignUnresolvedTypes(Type *);
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
    void assignUnresolvedTypes(Type *);
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
    void assignUnresolvedTypes(Type *);
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
    void assignUnresolvedTypes(Type *);
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
    void assignUnresolvedTypes(Type *);
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
    NodeExprProjection(int pos, int level): pos(pos), level(level), type(type) { }
    void rewriteDeclarations(SymbolTable *, NodeExpr **) { }
    void resolveSymbols(SymbolTable *);
    void rewriteFunctionApplications(NodeExpr **);
    void assignUnresolvedTypes(Type *);
    void compile(Assembly &);
    void compileL(Assembly &);
    Type *getType();

  private:
    int pos;
    int level;

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
    void assignUnresolvedTypes(Type *);
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
    NodeExprBinary(NodeExpr *lhs, NodeExpr *rhs): lhs(lhs), rhs(rhs), syms(0), type(0) {
      assert(lhs);
      assert(rhs);
    }

    void rewriteDeclarations(SymbolTable *, NodeExpr **);
    void resolveSymbols(SymbolTable *);
    void rewriteFunctionApplications(NodeExpr **);
    void assignUnresolvedTypes(Type *);
    void compile(Assembly &);
    void compileL(Assembly &);
    Type *getType() { return type; }
    std::string dump(int);
    
  protected:
    NodeExpr *lhs;
    NodeExpr *rhs;

    SymbolTable *syms;
    Type *type;

    virtual std::string op() = 0;
};

class NodeExprAssign: public NodeExprBinary {
  public:
    NodeExprAssign(NodeExpr *lhs, NodeExpr *rhs): NodeExprBinary(lhs, rhs) { }
    std::string op() { return "="; }

    void rewriteFunctionApplications(NodeExpr **);
    void assignUnresolvedTypes(Type *);
    void compile(Assembly &);
    void compileL(Assembly &);
};

class NodeExprLogicalAnd: public NodeExprBinary {
  public:
    NodeExprLogicalAnd(NodeExpr *lhs, NodeExpr *rhs): NodeExprBinary(lhs, rhs) { }
    std::string op() { return "&&"; }
};

class NodeExprLogicalOr: public NodeExprBinary {
  public:
    NodeExprLogicalOr(NodeExpr *lhs, NodeExpr *rhs): NodeExprBinary(lhs, rhs) { }
    std::string op() { return "||"; }
};

class NodeExprLogicalXor: public NodeExprBinary {
  public:
    NodeExprLogicalXor(NodeExpr *lhs, NodeExpr *rhs): NodeExprBinary(lhs, rhs) { }
    std::string op() { return "^^"; }
};

class NodeExprBitwiseAnd: public NodeExprBinary {
  public:
    NodeExprBitwiseAnd(NodeExpr *lhs, NodeExpr *rhs): NodeExprBinary(lhs, rhs) { }
    std::string op() { return "&"; }
};

class NodeExprBitwiseOr: public NodeExprBinary {
  public:
    NodeExprBitwiseOr(NodeExpr *lhs, NodeExpr *rhs): NodeExprBinary(lhs, rhs) { }
    std::string op() { return "|"; }
};

class NodeExprBitwiseXor: public NodeExprBinary {
  public:
    NodeExprBitwiseXor(NodeExpr *lhs, NodeExpr *rhs): NodeExprBinary(lhs, rhs) { }
    std::string op() { return "^"; }
};

class NodeExprEqual: public NodeExprBinary {
  public:
    NodeExprEqual(NodeExpr *lhs, NodeExpr *rhs): NodeExprBinary(lhs, rhs) { }
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

class NodeExprLessThan: public NodeExprBinary {
  public:
    NodeExprLessThan(NodeExpr *lhs, NodeExpr *rhs): NodeExprBinary(lhs, rhs) { }
    std::string op() { return "<"; }
};

class NodeExprLessThanEqual: public NodeExprBinary {
  public:
    NodeExprLessThanEqual(NodeExpr *lhs, NodeExpr *rhs): NodeExprBinary(lhs, rhs) { }
    std::string op() { return "<="; }
};

class NodeExprShiftLeft: public NodeExprBinary {
  public:
    NodeExprShiftLeft(NodeExpr *lhs, NodeExpr *rhs): NodeExprBinary(lhs, rhs) { }
    std::string op() { return "<<"; }
};

class NodeExprShiftRight: public NodeExprBinary {
  public:
    NodeExprShiftRight(NodeExpr *lhs, NodeExpr *rhs): NodeExprBinary(lhs, rhs) { }
    std::string op() { return ">>"; }
};

class NodeExprAdd: public NodeExprBinary {
  public:
    NodeExprAdd(NodeExpr *lhs, NodeExpr *rhs): NodeExprBinary(lhs, rhs) { }
    std::string op() { return "+"; }

    void rewriteFunctionApplications(NodeExpr **);
};

class NodeExprSub: public NodeExprBinary {
  public:
    NodeExprSub(NodeExpr *lhs, NodeExpr *rhs): NodeExprBinary(lhs, rhs) { }
    std::string op() { return "-"; }
};

class NodeExprMul: public NodeExprBinary {
  public:
    NodeExprMul(NodeExpr *lhs, NodeExpr *rhs): NodeExprBinary(lhs, rhs) { }
    std::string op() { return "*"; }
};

class NodeExprDiv: public NodeExprBinary {
  public:
    NodeExprDiv(NodeExpr *lhs, NodeExpr *rhs): NodeExprBinary(lhs, rhs) { }
    std::string op() { return "/"; }
};

class NodeExprMod: public NodeExprBinary {
  public:
    NodeExprMod(NodeExpr *lhs, NodeExpr *rhs): NodeExprBinary(lhs, rhs) { }
    std::string op() { return "%"; }
};

class NodeExprLoop: public NodeExpr {
  public:
    NodeExprLoop(NodeExpr *container, int tupleIndex, NodeExpr *expr)
      : container(container), tupleIndex(tupleIndex), expr(expr), type(0) { }

    void rewriteDeclarations(SymbolTable *, NodeExpr **);
    void resolveSymbols(SymbolTable *);
    void rewriteFunctionApplications(NodeExpr **);
    void assignUnresolvedTypes(Type *);
    void compile(Assembly &);
    void compileL(Assembly &);
    Type *getType();
    std::string dump(int i);

  private:
    NodeExpr *container;
    int tupleIndex;
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
    void assignUnresolvedTypes(Type *);
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
    void assignUnresolvedTypes(Type *) { assert(false); }
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
    void assignUnresolvedTypes(Type *);
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
