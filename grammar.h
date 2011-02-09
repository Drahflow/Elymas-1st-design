#ifndef GRAMMAR_H
#define GRAMMAR_H

#include "parser.h"
#include "treenodes.h"

#include <initializer_list>

class Grammar {
  public:
    Grammar();
    Parser<char, TreeNode *> *generateParser();

    Parser<char, TreeNode *> *resolveRule(const std::string &id) {
      auto parser = rules.find(id);
      if(parser == rules.end()) {
        return new Lazy<char, TreeNode *>(&rules[id]);
      }

      return new Lazy<char, TreeNode *>(&parser->second);
    }

    Parser<char, TreeNode *> *generateEqTs(const std::string &s);

    template<class R> void createRule(const std::string &lhs,
        const std::initializer_list<Parser<char, TreeNode *> *> &rhs,
        const R &red) {
      assert(rhs.size());

      Parser<char, std::vector<TreeNode *> > *option;
      option = reduction(*rhs.begin(),
          [](TreeNode *t) {
            return std::vector<TreeNode *>({ t });
          });

      auto skip_append = [](const std::pair<
          std::pair<std::vector<TreeNode *>, std::vector<char>>, TreeNode *> &p) {
        auto ret = p.first.first;
        ret.push_back(p.second);
        return ret;
      };

      for(auto i = rhs.begin() + 1; i != rhs.end(); ++i) {
        option = reduction(concat(concat(option, whitespace), *i), skip_append);
      }

      auto mapped = reduction(option, red);

      auto old = rules.find(lhs);
      if(old == rules.end() || !old->second) {
        rules[lhs] = mapped;
      } else {
        rules[lhs] = alternative(old->second, mapped);
      }
    }

    template<class R> void createRule(const std::string &lhs,
        Parser<char, TreeNode *> *p1,
        const R &red) {
      assert(p1);

      auto mapped = reduction(p1, red);

      auto old = rules.find(lhs);
      if(old == rules.end() || !old->second) {
        rules[lhs] = mapped;
      } else {
        rules[lhs] = alternative(old->second, mapped);
      }
    }

    template<class R> void createRule(const std::string &lhs,
        Parser<char, TreeNode *> *p1,
        Parser<char, TreeNode *> *p2,
        const R &red) {
      assert(p1);
      assert(p2);

      using namespace std;

      auto newred = [=](
          const pair<
                  pair<
                    TreeNode *,
                    std::vector<char>>,
                  TreeNode *> &p) {
        return red(
                p.first.first,
                p.second
              );
      };

      auto mapped = reduction(
            concat(concat(p1, whitespace), p2),
            newred);

      auto old = rules.find(lhs);
      if(old == rules.end() || !old->second) {
        rules[lhs] = mapped;
      } else {
        rules[lhs] = alternative(old->second, mapped);
      }
    }

    template<class R> void createRule(const std::string &lhs,
        Parser<char, TreeNode *> *p1,
        Parser<char, TreeNode *> *p2,
        Parser<char, TreeNode *> *p3,
        const R &red) {
      assert(p1);
      assert(p2);
      assert(p3);

      using namespace std;

      auto newred = [=](
          const pair<
                  pair<
                    pair<
                      pair<
                        TreeNode *,
                        std::vector<char>>,
                      TreeNode *>,
                    std::vector<char>>,
                  TreeNode *> &p) {
        return red(
                p.first.first.first.first,
                p.first.first.second,
                p.second
              );
      };

      auto mapped = reduction(
            concat(concat(concat(concat(p1, whitespace), p2), whitespace), p3),
            newred);

      auto old = rules.find(lhs);
      if(old == rules.end() || !old->second) {
        rules[lhs] = mapped;
      } else {
        rules[lhs] = alternative(old->second, mapped);
      }
    }

    void trivialRule(const std::string &lhs, const std::string &rhs) {
      auto rule = resolveRule(rhs);

      auto old = rules.find(lhs);
      if(old == rules.end() || !old->second) {
        rules[lhs] = rule;
      } else {
        rules[lhs] = alternative(old->second, rule);
      }
    }

    template<class N> void binaryExprRule(
        const std::string &name,
        const std::string &lhs,
        const std::string &op,
        const std::string &rhs) {
      createRule(name,
          resolveRule(lhs),
          generateEqTs(op),
          resolveRule(rhs),
        [](TreeNode *lhs, TreeNode *, TreeNode *rhs) -> TreeNode * {
          return new N(
            dynamic_cast<NodeExpr *>(lhs),
            dynamic_cast<NodeExpr *>(rhs));
        });
    }

    template<class N> void unaryExprRule(
        const std::string &name,
        const std::string &op,
        const std::string &arg) {
      createRule(name,
          generateEqTs(op),
          resolveRule(arg),
        [](TreeNode *, TreeNode *arg) -> TreeNode * {
          return new N(dynamic_cast<NodeExpr *>(arg));
        });
    }

    Parser<char, std::vector<char>> *getWhitespace() { return whitespace; }

  private:
    std::map<std::string, Parser<char, TreeNode *> *> rules;
    Parser<char, TreeNode *> *mandatory_whitespace;
    Parser<char, std::vector<char>> *whitespace;

    void selfTest();
    void selfTest(const std::string &rule, const std::string &input);

    void generateParserLambdas();
};

#endif
