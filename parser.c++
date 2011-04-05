#include "parser.h"

#include <iostream>

unsigned long Parser_creations = 0;

void testParseStream() {
  std::set<int> *set = new std::set<int>();
  set->insert(1);
  set->insert(7);
  set->insert(3);
  set->insert(9);

  auto test = new (class _: public Stream<std::pair<int, Stream<char> *> > {
    private:
      std::set<int> *set;
      std::set<int>::const_iterator i;

    public:
      _(std::set<int> *set, const std::set<int>::const_iterator &i)
        : set(set), i(i) { }
      std::pair<int, Stream<char> *> head() { return std::pair<int, Stream<char> *>(*i, Stream_empty()); }
      _ *tail() { return new _(set, ++i); }
      bool isEmpty() { return i == set->end(); }

  }) (set, set->begin());

  for(auto i = test; !i->isEmpty(); i = i->tail()) {
    std::cout << i->head().first << std::endl;
  }
}

int testParser(void) {
  testParseStream();

  std::string input = "xxxxxxxxxxxxxxxx";
  auto s = Stream_fromIterator(input.begin(), input.end());

  Parser<char, char> *X = new EqT<char> ('x');
  
  Parser<char, std::vector<char> > *XL = 
    alternative(
      reduction(
        concat(X, new Lazy<char, std::vector<char> >(&XL)),
        [](const std::pair<char, std::vector<char> > &x) {
          auto ret = x.second;
          ret.push_back(x.first);
          return ret;
        }),
      reduction(X, [](const char &x) { return std::vector<char>({ x }); })
      );

  auto parses = XL->parse(s);
  delete XL;

  if(parses->isEmpty()) {
    std::cout << "no parses found" << std::endl;
  }

  for(; !parses->isEmpty(); parses = parses->tail()) {
    std::cout << "parse step" << std::endl;
    auto v = parses->head();
    std::cout << "value: (length: " << v.first.size() << ")" << std::endl;
    for(auto i = v.first.begin(); i != v.first.end(); ++i) {
      std::cout << *i;
    }
    std::cout << std::endl;
    std::cout << "remainder: ";
    for(auto i = parses->head().second; !i->isEmpty(); i = i->tail()) {
      std::cout << i->head();
    }
    std::cout << std::endl;
  }

  delete parses;
  return 0;
}
