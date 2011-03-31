#include "grammar.h"

#include <tuple>
#include <iostream>
#include <functional>

Parser<char, char> *eqt(const unsigned char c) {
  static EqT<char> *eqts[256] = { 0 };

  Parser<char, char> *ret = eqts[c];
  if(!ret) {
    ret = eqts[c] = new EqT<char>(c);
  }

  return ret;
}

Parser<char, TreeNode *> *Grammar::generateEqTs(const std::string &s) {
  return reduction(
      new EqTs<char>(s),
      [](const int &i) -> TreeNode * {
        return 0;
      });
}

Grammar::Grammar() {
  mandatory_whitespace =
    reduction(new AnyEqT<char>({ ' ', '\t', '\n' }),
      [](const char &v) -> TreeNode * {
        return 0;
      });

  whitespace = repetition(new AnyEqT<char>({ ' ', '\t', '\n' }));
}

Parser<char, TreeNode *> *Grammar::generateParser() {
  rules["_identifier"] = 
    reduction(concat(
        new AnyEqT<char>({
            'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h',
            'i', 'j', 'k', 'l', 'm', 'n', 'o', 'p',
            'q', 'r', 's', 't', 'u', 'v', 'w', 'x',
            'y', 'z',
            'A', 'B', 'C', 'D', 'E', 'F', 'G', 'H',
            'I', 'J', 'K', 'L', 'M', 'N', 'O', 'P',
            'Q', 'R', 'S', 'T', 'U', 'V', 'W', 'X',
            'Y', 'Z',
            '_'
            }),
        repetition(new AnyEqT<char>({
            'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h',
            'i', 'j', 'k', 'l', 'm', 'n', 'o', 'p',
            'q', 'r', 's', 't', 'u', 'v', 'w', 'x',
            'y', 'z',
            'A', 'B', 'C', 'D', 'E', 'F', 'G', 'H',
            'I', 'J', 'K', 'L', 'M', 'N', 'O', 'P',
            'Q', 'R', 'S', 'T', 'U', 'V', 'W', 'X',
            'Y', 'Z',
            '_',
            '0', '1', '2', '3', '4', '5', '6', '7', '8', '9'
            }))),
        [](const std::pair<char, std::vector<char>> &s) -> TreeNode * {
          return new NodeIdentifier(s.first +
              std::string(s.second.rbegin(), s.second.rend()));
        });
  rules["_string"] =
    reduction(concat(concat(
        generateEqTs("\""),
        repetition(alternative(
            new NotEqT<char>('"'),
            reduction(
              concat(eqt('\\'), eqt('"')),
              [](const std::pair<char, char> &p) {
                return static_cast<char>(p.second);
              })))),
        generateEqTs("\"")),
      [](const std::pair<std::pair<TreeNode*, std::vector<char> >, TreeNode*> &str) -> TreeNode * {
        return new NodeString(std::string(str.first.second.rbegin(),
            str.first.second.rend()));
      });
  rules["_comment"] =
    reduction(concat(concat(
        generateEqTs("#"),
        repetition(new NotEqT<char>('\n'))),
        generateEqTs("\n")),
      [](const std::pair<std::pair<TreeNode*, std::vector<char> >, TreeNode*> &str) -> TreeNode * {
        return new NodeComment(std::string(str.first.second.rbegin(),
            str.first.second.rend()));
      });

  auto digit = new AnyEqT<char>({ '0', '1', '2', '3', '4', '5', '6', '7', '8', '9' });

  rules["_integer"] =
    alternative(
      reduction(concat(digit, repetition(digit)),
        [](const std::pair<char, std::vector<char>> &n) -> TreeNode * {
          unsigned long long val = n.first - '0';
          for(auto i = n.second.rbegin(); i != n.second.rend(); ++i) {
            val = 10 * val + (*i - '0');
          }
          return new NodeInteger(val);
        }),
      reduction(concat(generateEqTs("0x"),
          repetition(new AnyEqT<char>({
            '0', '1', '2', '3', '4',
            '5', '6', '7', '8', '9',
            'a', 'b', 'c', 'd', 'e', 'f',
            'A', 'B', 'C', 'D', 'E', 'F'
          }))),
        [](const std::pair<TreeNode *, std::vector<char>> &n) -> TreeNode * {
          unsigned long long val = 0;
          for(auto i = n.second.rbegin(); i != n.second.rend(); ++i) {
            val = 10 * val;
            if(*i >= '0' && *i <= '9') {
              val += *i - '0';
            } else if(*i >= 'a' && *i <= 'f') {
              val += 10 + *i - 'a';
            } else if(*i >= 'A' && *i <= 'F') {
              val += 10 + *i - 'A';
            }
          }
          return new NodeInteger(val);
        }));

  trivialRule("_expr_atom", "_identifier");
  trivialRule("_expr_atom", "_string");
  trivialRule("_expr_atom", "_integer");

  generateParserLambdas();

  trivialRule("_expr", "_expr_assign");

  trivialRule("_expr_assign", "_expr_lambda");
  binaryExprRule<NodeExprAssign>("_expr_assign",
      "_expr_lambda", "=", "_expr_assign");

  // ?: goes in between here

  trivialRule("_expr_lambda", "_expr_logical_or");
  createRule("_expr_lambda", {
      generateEqTs("λ"),
      resolveRule("_expr_tuple"),
      generateEqTs("→"),
      resolveRule("_expr_func_type"),
      resolveRule("_statement")
    }, [](const std::vector<TreeNode *> &v) -> TreeNode * {
      return new NodeTypedLambda(
        dynamic_cast<NodeExprTuple *>(v[1]),
        dynamic_cast<NodeExpr *>(v[3]),
        dynamic_cast<NodeStatement *>(v[4]));
    });

  trivialRule("_expr_logical_or", "_expr_logical_xor");
  binaryExprRule<NodeExprLogicalOr>("_expr_logical_or",
      "_expr_logical_xor", "||", "_expr_logical_or");

  trivialRule("_expr_logical_xor", "_expr_logical_and");
  binaryExprRule<NodeExprLogicalXor>("_expr_logical_xor",
      "_expr_logical_and", "^^", "_expr_logical_xor");

  trivialRule("_expr_logical_and", "_expr_bitwise_or");
  binaryExprRule<NodeExprLogicalAnd>("_expr_logical_and",
      "_expr_bitwise_or", "&&", "_expr_logical_and");

  trivialRule("_expr_bitwise_or", "_expr_bitwise_xor");
  binaryExprRule<NodeExprBitwiseOr>("_expr_bitwise_or",
      "_expr_bitwise_xor", "|", "_expr_bitwise_or");

  trivialRule("_expr_bitwise_xor", "_expr_bitwise_and");
  binaryExprRule<NodeExprBitwiseXor>("_expr_bitwise_xor",
      "_expr_bitwise_and", "^", "_expr_bitwise_xor");

  trivialRule("_expr_bitwise_and", "_expr_comp_1");
  binaryExprRule<NodeExprBitwiseAnd>("_expr_bitwise_and",
      "_expr_comp_1", "&", "_expr_bitwise_and");

  trivialRule("_expr_comp_1", "_expr_comp_2");
  binaryExprRule<NodeExprEqual>("_expr_comp_1",
      "_expr_comp_2", "==", "_expr_comp_1");
  createRule("_expr_comp_1",
      resolveRule("_expr_comp_2"),
      generateEqTs("!="),
      resolveRule("_expr_comp_1"),
    [](TreeNode *lhs, TreeNode *, TreeNode *rhs) -> TreeNode * {
      return new NodeExprLogicalNot(new NodeExprEqual(
        dynamic_cast<NodeExpr *>(lhs),
        dynamic_cast<NodeExpr *>(rhs)));
    });

  trivialRule("_expr_comp_2", "_expr_shift");
  binaryExprRule<NodeExprLessThan>("_expr_comp_2",
      "_expr_shift", "<", "_expr_comp_2");
  binaryExprRule<NodeExprLessThanEqual>("_expr_comp_2",
      "_expr_shift", "<=", "_expr_comp_2");
  createRule("_expr_comp_2",
      resolveRule("_expr_shift"),
      generateEqTs(">"),
      resolveRule("_expr_comp_2"),
    [](TreeNode *lhs, TreeNode *, TreeNode *rhs) -> TreeNode * {
      return new NodeExprLessThan(
        dynamic_cast<NodeExpr *>(rhs),
        dynamic_cast<NodeExpr *>(lhs));
    });
  createRule("_expr_comp_2",
      resolveRule("_expr_shift"),
      generateEqTs(">="),
      resolveRule("_expr_comp_2"),
    [](TreeNode *lhs, TreeNode *, TreeNode *rhs) -> TreeNode * {
      return new NodeExprLessThanEqual(
        dynamic_cast<NodeExpr *>(rhs),
        dynamic_cast<NodeExpr *>(lhs));
    });

  trivialRule("_expr_shift", "_expr_add");
  binaryExprRule<NodeExprShiftLeft>("_expr_shift",
      "_expr_add", "<<", "_expr_shift");
  binaryExprRule<NodeExprShiftRight>("_expr_shift",
      "_expr_add", ">>", "_expr_shift");

  trivialRule("_expr_add", "_expr_mul");
  binaryExprRule<NodeExprAdd>("_expr_add",
      "_expr_mul", "+", "_expr_add");
  binaryExprRule<NodeExprSub>("_expr_add",
      "_expr_mul", "-", "_expr_add");

  trivialRule("_expr_mul", "_expr_unary_minus");
  binaryExprRule<NodeExprMul>("_expr_mul",
      "_expr_unary_minus", "*", "_expr_mul");
  binaryExprRule<NodeExprDiv>("_expr_mul",
      "_expr_unary_minus", "/", "_expr_mul");
  binaryExprRule<NodeExprMod>("_expr_mul",
      "_expr_unary_minus", "%", "_expr_mul");

  trivialRule("_expr_unary_minus", "_expr_apply");
  unaryExprRule<NodeExprUnaryMinus>("_expr_unary_minus",
      "-", "_expr_apply");
 
  trivialRule("_expr_apply", "_expr_func_type");
  createRule("_expr_apply",
      resolveRule("_expr_func_type"),
      mandatory_whitespace,
      resolveRule("_expr_apply"),
    [](TreeNode *func, TreeNode *, TreeNode *args) -> TreeNode * {
      return new NodeExprApply(
        dynamic_cast<NodeExpr *>(func),
        dynamic_cast<NodeExpr *>(args));
    });

  trivialRule("_expr_func_type", "_expr_atom");
  createRule("_expr_func_type",
      resolveRule("_expr_atom"),
      generateEqTs("→"),
      resolveRule("_expr_func_type"),
    [](TreeNode *lhs, TreeNode *, TreeNode *rhs) -> TreeNode * {
      return new NodeTypeFunction(
        dynamic_cast<NodeExpr *>(lhs),
        dynamic_cast<NodeExpr *>(rhs));
    });

  createRule("_expr_list",
      resolveRule("_expr"),
    [](TreeNode *expr) -> TreeNode * {
      return new NodeExprList(dynamic_cast<NodeExpr *>(expr));
    });
  createRule("_expr_list",
      resolveRule("_expr"),
      generateEqTs(","),
      resolveRule("_expr_list"),
    [](TreeNode *head, TreeNode *, TreeNode *list) -> TreeNode * {
      return new NodeExprList(
        dynamic_cast<NodeExpr *>(head),
        dynamic_cast<NodeExprList *>(list));
    });
  createRule("_expr_tuple",
      generateEqTs("("),
      resolveRule("_expr_list"),
      generateEqTs(")"),
    [](TreeNode *, TreeNode *list, TreeNode *) -> TreeNode * {
      return new NodeExprTuple(dynamic_cast<NodeExprList *>(list));
    });
  trivialRule("_expr_atom", "_expr_tuple");

  createRule("_expr_array",
      generateEqTs("["),
      resolveRule("_expr_list"),
      generateEqTs("]"),
    [](TreeNode *, TreeNode *list, TreeNode *) -> TreeNode * {
      return new NodeExprArray(dynamic_cast<NodeExprList *>(list));
    });
  trivialRule("_expr_atom", "_expr_array");

  createRule("_statement",
      resolveRule("_expr"),
      generateEqTs(";"),
    [](TreeNode *expr, TreeNode *) -> TreeNode * {
      return new NodeStatementExpr(dynamic_cast<NodeExpr *>(expr));
    });
  createRule("_statement",
      generateEqTs("{"),
      resolveRule("_statement_list"),
      generateEqTs("}"),
    [](TreeNode *, TreeNode *list, TreeNode *) -> TreeNode * {
      return new NodeStatementBlock(dynamic_cast<NodeStatementList *>(list));
    });
  createRule("_statement",
      generateEqTs("{"),
      generateEqTs("}"),
    [](TreeNode *, TreeNode *) -> TreeNode * {
      return new NodeStatementBlock();
    });
  createRule("_statement",
      resolveRule("_comment"),
    [](TreeNode *) -> TreeNode * {
      return new NodeStatementBlock();
    });

  createRule("_statement", {
      generateEqTs("while"),
      generateEqTs("("),
      resolveRule("_expr"),
      generateEqTs(")"),
      resolveRule("_statement")
    }, [](const std::vector<TreeNode *> &v) -> TreeNode * {
      return new NodeStatementWhile(
        dynamic_cast<NodeExpr *>(v[2]),
        dynamic_cast<NodeStatement *>(v[4]));
    });
  createRule("_statement",
      generateEqTs("goto"),
      resolveRule("_expr"),
      generateEqTs(";"),
    [](TreeNode *, TreeNode *expr, TreeNode *) -> TreeNode * {
      return new NodeStatementGoto(
        dynamic_cast<NodeExpr *>(expr));
    });
  createRule("_statement",
      resolveRule("_identifier"),
      generateEqTs(":"),
    [](TreeNode *id, TreeNode *) -> TreeNode * {
      return new NodeStatementLabel(
        dynamic_cast<NodeIdentifier *>(id));
    });

  createRule("_statement_list",
    resolveRule("_statement"),
    resolveRule("_statement_list"),
    [](TreeNode *head, TreeNode *list) -> TreeNode * {
      return new NodeStatementList(
        dynamic_cast<NodeStatement *>(head),
        dynamic_cast<NodeStatementList *>(list));
    });
  createRule("_statement_list",
    resolveRule("_statement"),
    [](TreeNode *stmt) -> TreeNode * {
      return new NodeStatementList(
        dynamic_cast<NodeStatement *>(stmt));
    });

  createRule("_statement", {
      generateEqTs("syntax"),
      resolveRule("_identifier"),
      resolveRule("_expr"),
      resolveRule("_statement"),
      generateEqTs(";")
    }, [](const std::vector<TreeNode *> &v) -> TreeNode * {
      return new NodeStatementRule(
        dynamic_cast<NodeIdentifier *>(v[1]),
        dynamic_cast<NodeExprTuple *>(v[2]),
        dynamic_cast<NodeStatement *>(v[3]));
    });

  // selfTest();

  return resolveRule("_statement");
}

void Grammar::generateParserLambdas() {
  std::vector<std::string> greek = {
    "α", "β", "γ", "δ", "ε", "ζ", "η", "θ", "ι", "κ"
  };
  std::vector<std::string> subscript = {
    "", "₁", "₂", "₃", "₄", "₅", "₆", "₇", "₈", "₉"
  };
  
  int l = 0;
  for(auto k = subscript.begin(); k != subscript.end(); ++k, ++l) {
    int j = 0;
    for(auto i = greek.begin(); i != greek.end(); ++i, ++j) {
      auto f =
        [=](const std::vector<TreeNode *> &v) -> TreeNode * {
          return new NodeExprProjection(j, l);
        };

      auto fp = new decltype(f)(f);

      createRule("_expr_atom", {
          generateEqTs(*i + *k)
        }, *fp);
    }
  }
}

void Grammar::selfTest(const std::string &rule, const std::string &input) {
  Stream<char> *identstream = Stream_fromIterator(input.begin(), input.end());

  auto p = rules[rule]->parseFull(identstream);
  if(p->isEmpty()) {
    std::cerr << "FAILED: no parse for rule " << rule <<
      " with input '" << input << "'" << std::endl;
  } else {
    std::cerr << "successful parse for rule " << rule << std::endl;
  }
}

void Grammar::selfTest() {
  selfTest("_identifier", "_");
  selfTest("_identifier", "i");
  selfTest("_identifier", "hallo");
  selfTest("_identifier", "WTF");
  selfTest("_string", "\"\"");
  selfTest("_string", "\"a\"");
  selfTest("_string", "\"\\\"\"");
  selfTest("_string", "\"abc\"");
  selfTest("_integer", "0");
  selfTest("_integer", "11");
  selfTest("_integer", "15532750637253");
  selfTest("_integer", "0x");
  selfTest("_integer", "0x0");
  selfTest("_integer", "0x53109743014");
  selfTest("_integer", "0x53abc743ABC");
  selfTest("_expr", "_");
  selfTest("_expr", "i");
  selfTest("_expr", "hallo");
  selfTest("_expr", "WTF");
  selfTest("_expr_list", "i");
  selfTest("_expr_list", "i, i");
  selfTest("_expr_list", "i, i, i");
  selfTest("_expr", "(i, i, i)");
  selfTest("_expr", "(i, β, α₂)");
  selfTest("_expr", "a = b");
  selfTest("_expr", "a = f 0");
  selfTest("_expr", "g f 0");
  selfTest("_expr", "α β (0, 1)");
  selfTest("_expr", "a && b");
  selfTest("_expr", "a && b && c");
  selfTest("_expr", "a || b");
  selfTest("_expr", "a && b || c ^^ d");
  selfTest("_expr", "a & b | c ^ d");
  selfTest("_expr", "a == b != c");
  selfTest("_expr", "a <= b < c > d >= e");
  selfTest("_expr", "a << b >> c");
  selfTest("_expr", "a + b - c");
  selfTest("_expr", "a * b / c % c");
  selfTest("_statement_list", "0;");
  selfTest("_statement_list", "0; 1;");
  selfTest("_statement", "0;");
  selfTest("_statement", "{}");
  selfTest("_statement", "{ }");
  selfTest("_statement", "syntax i (i, i, i) { };");
  selfTest("_statement", "goto target;");
  selfTest("_statement", "target:");
  selfTest("_statement", "while(0) goto target;");
}
