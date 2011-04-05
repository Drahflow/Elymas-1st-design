// Shamelessly stolen and translated from:
// http://www.ucombinator.org/projects/parsing/Parser.scala

#ifndef PARSER_H
#define PARSER_H

#include <vector>
#include <set>
#include <unordered_set>
#include <list>
#include <string>
#include <map>
#include <stdexcept>
#include <algorithm>
#include <memory>
#include <iostream>
#include <assert.h>
#include <strings.h>

template<class T> class Stream {
  public:
    virtual T head() = 0;
    virtual Stream *tail() = 0;
    virtual bool isEmpty() = 0;
    virtual ~Stream() { };
};

template<class T> Stream<T> *Stream_cons(const T &t, Stream<T> *tail) {
  return new (class _: public Stream<T> {
    private:
      T t;
      Stream<T> *tail_;

    public:
      _(const T &t, Stream<T> *tail): t(t), tail_(tail) { }
      T head() { return t; }
      Stream<T> *tail() { return tail_; }
      bool isEmpty() { return false; }
  }) (t, tail);
}

struct Stream_untyped {
  template<class T> operator Stream<T> *() const {
    return new (class _: public Stream<T> {
      public:
        T head() { throw std::logic_error("should never be called"); }
        Stream<T> *tail() { return 0; }
        bool isEmpty() { return true; }
    }) ();
  }
};

template<class T> Stream<T> *Stream_cons(const T &t, const Stream_untyped &tail) {
  return Stream_cons(t, static_cast<Stream<T> *>(tail));
}

inline Stream_untyped Stream_empty() { return Stream_untyped(); }

template<class I> Stream<typename I::value_type> *Stream_fromIterator(const I &begin, const I &end) {
  return new (class _: public Stream<typename I::value_type> {
    private:
      I begin;
      I end;

    public:
      _(const I &begin, const I &end): begin(begin), end(end) { }
      typename I::value_type head() { return *begin; }
      _ *tail() { I next = begin; return new _(++next, end); }
      bool isEmpty() { return begin == end; }
  }) (begin, end);
}

class ChangeCell {
  public:
    bool changed;

    void orWith(bool change) {
      changed = changed || change;
    }

    ChangeCell(): changed(false) { }
};

template<class T, class A> class Emp;
template<class T, class A> class Eps;
template<class T, class A> class Rep;
template<class T, class A> class Alt;
template<class T, class A> class Lazy;
template<class T, class A, class B, class F> class Red;
template<class T, class A, class B> class Con;

template<class T> std::ostream &operator << (std::ostream &o,
    const std::vector<T> &v) {
  o << "[";
  for(auto i = v.begin(); i != v.end(); ++i) {
    o << *i << ",";
  }
  o << "]";
  return o;
}

template<class X, class Y> std::ostream &operator << (std::ostream &o,
    const std::pair<X, Y> &p) {
  return o << "<" << p.first << "," << p.second << ">";
}

extern unsigned long Parser_creations;

template<class T, class A> class Parser {
  private:
    std::set<A> parseNullLocal;
    bool isEmptyLocal;
    bool isNullableLocal;

    Parser(): isEmptyLocal(false), isNullableLocal(false), initialized(false) {
      ++Parser_creations;
    }

  protected:
    Parser(bool empty, bool nullable)
      : isEmptyLocal(empty), isNullableLocal(nullable), initialized(false) {
      ++Parser_creations;
    }

  public:
    std::set<A> parseNull() {
      if(isEmpty()) return std::set<A>();

      init();
      return parseNullLocal;
    }

    bool isNullable() {
      if(isEmpty()) return false;

      init();
      return isNullableLocal;
    }

    bool isEmpty() {
      init();
      return isEmptyLocal;
    }

  protected:
    bool setParseNull(std::set<A> set) {
      if(parseNullLocal.size() == set.size()) return false;
      //std::cout << "changing null parse set" << std::endl;
      //std::cout << "old set size: " << parseNullLocal.size() << std::endl;
      //for(auto i = parseNullLocal.begin(); i != parseNullLocal.end(); ++i) {
      //  std::cout << "  " << *i << std::endl;
      //}
      //std::cout << "new set size: " << set.size() << std::endl;
      //for(auto i = set.begin(); i != set.end(); ++i) {
      //  std::cout << "  " << *i << std::endl;
      //}

      parseNullLocal = set;
      return true;
    }

    bool setIsEmpty(bool v) {
      if(isEmptyLocal == v) return false;
      //std::cout << "changing is empty" << std::endl;

      isEmptyLocal = v;
      return true;
    }

    bool setIsNullable(bool v) {
      if(isNullableLocal == v) return false;
      //std::cout << "changing is nullable" << std::endl;

      isNullableLocal = v;
      return true;
    }

    bool initialized;

  private:
    void init() {
      if(initialized) return;
      initialized = true;

      ChangeCell change;
      //long i = 0;
      //std::cerr << "begun anew" << std::endl;
      do {
        change = ChangeCell();
        updateChildBasedAttributes(change);
        //++i;
        //std::cerr << "and another round" << std::endl;
      } while(change.changed);
      //std::cerr << "terminated after " << i << std::endl;
    }

  public:
    virtual void updateChildBasedAttributes(ChangeCell &change) { }

  protected:
    virtual Parser<T, A> *internalDerive(T t) = 0;

  private:
    std::map<T, Parser<T, A> *> cache;

  public:
    virtual Parser<T, A> *derive(T t) {
      static Emp<T, A> fail;

      if(isEmpty()) {
        return &fail;
      } else if(cache.find(t) != cache.end()) {
        assert(cache[t]);

        return cache[t];
      } else {
//        std::cerr << "internal derive for char: " << t
//          << " for " << typeid(*this).name()
//          << "@" << this
//          << std::endl;
        auto tmp = internalDerive(t);
        assert(tmp);

        return cache[t] = tmp;
      }
    }

    virtual Stream<A> *parseFull(Stream<T> *input) {
      if(input->isEmpty()) {
        auto nullSet = new std::set<A>(parseNull());
        return Stream_fromIterator(nullSet->begin(), nullSet->end());
      }

      return derive(input->head())->parseFull(input->tail());
    }

    virtual Stream<std::pair<A, Stream<T> *> > *parse(Stream<T> *input) {
      if(input->isEmpty()) {
        std::set<A> *set = new std::set<A>(parseNull());

        return new (class _: public Stream<std::pair<A, Stream<T> *> > {
          private:
            std::set<A> *set;
            typename std::set<A>::const_iterator i;

          public:
            _(std::set<A> *set, const typename std::set<A>::const_iterator &i): set(set), i(i) { }
            std::pair<A, Stream<T> *> head() { return std::make_pair(*i, Stream_empty()); }
            _ *tail() { auto j = i; return new _(set, ++j); }
            bool isEmpty() { return i == set->end(); }
        }) (set, set->begin());
      }

      return combineEven(
          derive(input->head())->parse(input->tail()),
          new (class _: public Stream<std::pair<A, Stream<T> *> > {
            private:
              Stream<T> *input;
              Stream<A> *full;

            public:
              _(Stream<T> *input, Stream<A> *full): input(input), full(full) { }
              std::pair<A, Stream<T> *> head() { return std::make_pair(full->head(), input); }
              Stream<std::pair<A, Stream<T> *> > *tail() { return new _(input, full->tail()); }
              bool isEmpty() { return full->isEmpty(); }
          }) (input, parseFull(Stream_empty()))
        );
    }

  private:
    template<class X> Stream<X> *combineEven(Stream<X> *s1, Stream<X> *s2) {
      if(!s1->isEmpty()) return Stream_cons(s1->head(), combineOdd(s1->tail(), s2));
      if(!s2->isEmpty()) return Stream_cons(s2->head(), combineOdd(s1, s2->tail()));
      return Stream_empty();
    }

    template<class X> Stream<X> *combineOdd(Stream<X> *s1, Stream<X> *s2) {
      if(!s2->isEmpty()) return Stream_cons(s2->head(), combineEven(s1, s2->tail()));
      if(!s1->isEmpty()) return Stream_cons(s1->head(), combineEven(s1->tail(), s2));
      return Stream_empty();
    }

    friend class Lazy<T, A>;
};

template<class A> class Parser<char, A> {
  private:
    std::set<A> parseNullLocal;
    bool isEmptyLocal;
    bool isNullableLocal;

    Parser(): isEmptyLocal(false), isNullableLocal(false), initialized(false) {
      ++Parser_creations;

      bzero(cache, sizeof(cache));
    }

  protected:
    Parser(bool empty, bool nullable)
      : isEmptyLocal(empty), isNullableLocal(nullable), initialized(false) {
      ++Parser_creations;

      bzero(cache, sizeof(cache));
    }

  public:
    std::set<A> parseNull() {
      if(isEmpty()) return std::set<A>();

      init();
      return parseNullLocal;
    }

    bool isNullable() {
      if(isEmpty()) return false;

      init();
      return isNullableLocal;
    }

    bool isEmpty() {
      init();
      return isEmptyLocal;
    }

  protected:
    bool setParseNull(std::set<A> set) {
      if(parseNullLocal.size() == set.size()) return false;
      //std::cout << "changing null parse set" << std::endl;
      //std::cout << "old set size: " << parseNullLocal.size() << std::endl;
      //for(auto i = parseNullLocal.begin(); i != parseNullLocal.end(); ++i) {
      //  std::cout << "  " << *i << std::endl;
      //}
      //std::cout << "new set size: " << set.size() << std::endl;
      //for(auto i = set.begin(); i != set.end(); ++i) {
      //  std::cout << "  " << *i << std::endl;
      //}

      parseNullLocal = set;
      return true;
    }

    bool setIsEmpty(bool v) {
      if(isEmptyLocal == v) return false;
      //std::cout << "changing is empty" << std::endl;

      isEmptyLocal = v;
      return true;
    }

    bool setIsNullable(bool v) {
      if(isNullableLocal == v) return false;
      //std::cout << "changing is nullable" << std::endl;

      isNullableLocal = v;
      return true;
    }

    bool initialized;

  private:
    void init() {
      if(initialized) return;
      initialized = true;

      ChangeCell change;
      //long i = 0;
      //std::cerr << "begun anew" << std::endl;
      do {
        change = ChangeCell();
        updateChildBasedAttributes(change);
        //++i;
        //std::cerr << "and another round" << std::endl;
      } while(change.changed);
      //std::cerr << "terminated after " << i << std::endl;
    }

  public:
    virtual void updateChildBasedAttributes(ChangeCell &) { }

  protected:
    virtual Parser<char, A> *internalDerive(char t) = 0;

  private:
    Parser<char, A> *cache[256];

  public:
    virtual Parser<char, A> *derive(char t) {
      static Emp<char, A> fail;

      if(isEmpty()) {
        return &fail;
      } else if(cache[static_cast<unsigned char>(t)]) {
        return cache[static_cast<unsigned char>(t)];
      } else {
//        std::cerr << "internal derive for char: " << t
//          << " for " << typeid(*this).name()
//          << "@" << this
//          << std::endl;
        auto tmp = internalDerive(t);
        assert(tmp);

        return cache[static_cast<unsigned char>(t)] = tmp;
      }
    }

    virtual Stream<A> *parseFull(Stream<char> *input) {
      if(input->isEmpty()) {
        auto nullSet = new std::set<A>(parseNull());
        return Stream_fromIterator(nullSet->begin(), nullSet->end());
      }

      return derive(input->head())->parseFull(input->tail());
    }

    virtual Stream<std::pair<A, Stream<char> *> > *parse(Stream<char> *input) {
      if(input->isEmpty()) {
        std::set<A> *set = new std::set<A>(parseNull());

        return new (class _: public Stream<std::pair<A, Stream<char> *> > {
          private:
            std::set<A> *set;
            typename std::set<A>::const_iterator i;

          public:
            _(std::set<A> *set, const typename std::set<A>::const_iterator &i): set(set), i(i) { }
            std::pair<A, Stream<char> *> head() { return std::make_pair(*i, Stream_empty()); }
            _ *tail() { auto j = i; return new _(set, ++j); }
            bool isEmpty() { return i == set->end(); }
        }) (set, set->begin());
      }

      return combineEven(
          derive(input->head())->parse(input->tail()),
          new (class _: public Stream<std::pair<A, Stream<char> *> > {
            private:
              Stream<char> *input;
              Stream<A> *full;

            public:
              _(Stream<char> *input, Stream<A> *full): input(input), full(full) { }
              std::pair<A, Stream<char> *> head() { return std::make_pair(full->head(), input); }
              Stream<std::pair<A, Stream<char> *> > *tail() { return new _(input, full->tail()); }
              bool isEmpty() { return full->isEmpty(); }
          }) (input, parseFull(Stream_empty()))
        );
    }

  private:
    template<class X> Stream<X> *combineEven(Stream<X> *s1, Stream<X> *s2) {
      if(!s1->isEmpty()) return Stream_cons(s1->head(), combineOdd(s1->tail(), s2));
      if(!s2->isEmpty()) return Stream_cons(s2->head(), combineOdd(s1, s2->tail()));
      return Stream_empty();
    }

    template<class X> Stream<X> *combineOdd(Stream<X> *s1, Stream<X> *s2) {
      if(!s2->isEmpty()) return Stream_cons(s2->head(), combineEven(s1, s2->tail()));
      if(!s1->isEmpty()) return Stream_cons(s1->head(), combineEven(s1->tail(), s2));
      return Stream_empty();
    }

    friend class Lazy<char, A>;
};

template<class T, class Composite, class F> auto reduction(
    Parser<T, Composite> *parser, const F &f) -> Red<T, Composite, decltype(f(*(Composite*)0)), F> *{
  assert(parser);

  return new Red<T, Composite, decltype(f(*(Composite*)0)), F>(parser, f);
}

template<class T, class A> Alt<T, A> *alternative(Parser<T, A> *a, Parser<T, A> *b) {
  assert(a);
  assert(b);

  return new Alt<T, A>(a, b);
}

template<class T, class A> Rep<T, A> *repetition(Parser<T, A> *parser) {
  assert(parser);

  return new Rep<T, A>(parser);
}

template<class T, class A, class B> Parser<T, std::pair<A, B> > *concat(Parser<T, A> *a, Parser<T, B> *b) {
  assert(a);
  assert(b);

  return new Con<T, A, B>(a, b);
}

template<class T, class A> class Lazy: public Parser<T, A> {
  private:
    Parser<T, A> **parser;

  public:
    Lazy(Parser<T, A>** parser): Parser<T, A>(false, false), parser(parser) { }

    virtual void updateChildBasedAttributes(ChangeCell &change) {
      (*parser)->updateChildBasedAttributes(change);
    }

    virtual Parser<T, A> *internalDerive(T t) {
      return (*parser)->internalDerive(t);
    }

    virtual Parser<T, A> *derive(T t) {
      return (*parser)->derive(t);
    }

    virtual Stream<A> *parseFull(Stream<T> *input) {
      return (*parser)->parseFull(input);
    }

    virtual Stream<std::pair<A, Stream<T> *> > *parse(Stream<T> *input) {
      return (*parser)->parse(input);
    }
};

template<class T> class EqT: public Parser<T, T> {
  private:
    T t;

  public:
    EqT(T t): Parser<T, T>(false, false), t(t) { }

    Parser<T, T> *internalDerive(T t_) {
      static Emp<T, T> fail;

      if(t == t_) return new Eps<T, T>(Stream_cons(t_, Stream_empty()));
      return &fail;
    }

    Stream<std::pair<T, Stream<T> *> > *parse(Stream<T> *input) {
      if(input->isEmpty()) return Stream_empty();
      if(input->head() == t) return Stream_cons(
          std::make_pair(input->head(), input->tail()), Stream_empty());
      return Stream_empty();
    }
};

template<class T> class EqTs: public Parser<T, int> {
  private:
    std::basic_string<T, std::char_traits<T>> s;

  public:
    EqTs(const std::basic_string<T, std::char_traits<T>> &s)
        : Parser<T, int>(false, s.empty()), s(s) {
      assert(s.length());
    }

    Parser<T, int> *internalDerive(T t) {
      static Emp<T, int> fail;

      if(t == s[0]) {
        if(s.length() == 1) {
          return new Eps<T, int>(Stream_cons(0, Stream_empty()));
        } else {
          return new EqTs<T>(s.substr(1));
        }
      } else {
        return &fail;
      }
    }

    Stream<std::pair<int, Stream<T> *> > *parse(Stream<T> *input) {
      auto i = s.begin();
      while(i != s.end()) {
        if(input->isEmpty()) return Stream_empty();
        if(input->head() != *i) return Stream_empty();
        input = input->tail();
      }

      return Stream_cons(std::make_pair(0, input->tail()), Stream_empty());
    }
};

template<class T> class NotEqT: public Parser<T, T> {
  private:
    T t;

  public:
    NotEqT(T t): Parser<T, T>(false, false), t(t) { }

  Parser<T, T> *internalDerive(T t_) {
    static Emp<T, T> fail;

    if(t != t_) return new Eps<T, T>(Stream_cons(t_, Stream_empty()));
    return &fail;
  }

  Stream<std::pair<T, Stream<T> *> > *parse(Stream<T> *input) {
    if(input->isEmpty()) return Stream_empty();
    if(input->head() != t) return Stream_cons(
        std::make_pair(input->head(), input->tail()), Stream_empty());
    return Stream_empty();
  }
};

template<class T> class AnyEqT: public Parser<T, T> {
  private:
    std::set<T> t;

  public:
    AnyEqT(const std::set<T> &t): Parser<T, T>(false, false), t(t) { }

  Parser<T, T> *internalDerive(T t_) {
    static Emp<T, T> fail;

    if(t.find(t_) != t.end()) {
      return new Eps<T, T>(Stream_cons(t_, Stream_empty()));
    }

    return &fail;
  }

  Stream<std::pair<T, Stream<T> *> > *parse(Stream<T> *input) {
    if(input->isEmpty()) return Stream_empty();
    if(t.find(input->head()) != t.end()) return Stream_cons(
        std::make_pair(input->head(), input->tail()), Stream_empty());
    return Stream_empty();
  }
};

template<class T, class A> class Emp: public Parser<T, A> {
  public:
    Emp(): Parser<T, A>(true, false) { }

  protected:
    Parser<T, A> *internalDerive(T) {
      return this;
    }

    Stream<std::pair<A, Stream<T> *> > *parse(Stream<T> *) {
      return Stream_empty();
    }
};

template<class T, class A> class Eps: public Parser<T, A> {
  private:
    Stream<A> *generator;

  public:
    Eps(Stream<A> *generator): Parser<T, A>(false, true), generator(generator) { }

  protected:
    Parser<T, A> *internalDerive(T) {
      static Emp<T, A> fail;
      return &fail;
    }

    Stream<std::pair<A, Stream<T> *> > *parse(Stream<T> *input) {
      return new (class _: public Stream<std::pair<A, Stream<T> *> > {
          private:
            Stream<A> *generator;
            Stream<T> *input;

          public:
            _(Stream<A> *generator, Stream<T> *input)
              : generator(generator), input(input) { }
            std::pair<A, Stream<T> *> head() { return std::make_pair(generator->head(), input); }
            _ *tail() { return new _(generator->tail(), input); }
            bool isEmpty() { return generator->isEmpty() ;}
      }) (generator, input);
    }

    void updateChildBasedAttributes(ChangeCell &change) {
      std::set<A> generated;
      for(Stream<A> *i = generator; !i->isEmpty(); i = i->tail()) {
        generated.insert(i->head());
      }

      change.orWith(setParseNull(generated));
    }
};

template<class T, class A, class B> class Con: public Parser<T, std::pair<A, B> > {
  public:
    Con(Parser<T, A> *first, Parser<T, B> *second)
      : Parser<T, std::pair<A, B> >(false, false),
        first(first), second(second)
    {
      assert(first);
      assert(second);
    }

  private:
    Parser<T, A> *first;
    Parser<T, B> *second;

  protected:
    Parser<T, std::pair<A, B> > *internalDerive(T t) {
      if(first->isNullable()) {
        return alternative(concat(first->derive(t), second),
            concat(new Eps<T, A>(new (class _: public Stream<A> {
                    private:
                      Stream<std::pair<A, Stream<T> *> > *src;

                    public:
                      _(Stream<std::pair<A, Stream<T> *> > *src): src(src) { }
                      A head() { return src->head().first; }
                      _ *tail() { return new _(src->tail()); }
                      bool isEmpty() { return src->isEmpty(); }
                  }) (first->parse(Stream_empty()))),
              second->derive(t)));
      }

      return concat(first->derive(t), second);
    }

    void updateChildBasedAttributes(ChangeCell &change) {
      if(!this->initialized) {
        this->initialized = true;
        first->updateChildBasedAttributes(change);
        second->updateChildBasedAttributes(change);
      }

      std::set<std::pair<A, B> > both;
      std::set<A> firstSet = first->parseNull();
      std::set<B> secondSet = second->parseNull();
      for(auto i = firstSet.begin(); i != firstSet.end(); ++i) {
        for(auto j = secondSet.begin(); j != secondSet.end(); ++j) {
          both.insert(std::make_pair(*i, *j));
        }
      }
      change.orWith(setParseNull(both));
      change.orWith(setIsEmpty(first->isEmpty() || second->isEmpty()));
      change.orWith(setIsNullable(!this->isEmpty() && first->isNullable() && second->isNullable()));
    }
};

template<class T, class A> class Alt: public Parser<T, A> {
  public:
    Alt(Parser<T, A> *choice1, Parser<T, A> *choice2)
      : Parser<T, A>(false, false), choice1(choice1), choice2(choice2) {
      assert(choice1);
      assert(choice2);
    }

  private:
    Parser<T, A> *choice1;
    Parser<T, A> *choice2;

  protected:
    Parser<T, A> *internalDerive(T t) {
      if(choice1->isEmpty()) return choice2->derive(t);
      if(choice2->isEmpty()) return choice1->derive(t);
      return new Alt(choice1->derive(t), choice2->derive(t));
    }

    void updateChildBasedAttributes(ChangeCell &change) {
      if(!this->initialized) {
        this->initialized = true;
        choice1->updateChildBasedAttributes(change);
        choice2->updateChildBasedAttributes(change);
      }

      std::set<A> firstSet = choice1->parseNull();
      std::set<A> secondSet = choice2->parseNull();
      for(auto j = secondSet.begin(); j != secondSet.end(); ++j) {
        firstSet.insert(*j);
      }
      change.orWith(setParseNull(firstSet));
      change.orWith(setIsEmpty(choice1->isEmpty() && choice2->isEmpty()));
      change.orWith(setIsNullable(!this->isEmpty() && (choice1->isNullable() || choice2->isNullable())));
    }
};

template<class T, class A> class Rep: public Parser<T, std::vector<A> > {
  public:
    Rep(Parser<T, A> *parser)
      : Parser<T, std::vector<A> >(false, true), parser(parser) { }

  private:
    Parser<T, A> *parser;

  protected:
    Parser<T, std::vector<A> > *internalDerive(T t) {
      return reduction(concat(parser->derive(t), this),
        [](const std::pair<A, std::vector<A> > &a) {
          auto ret = a.second;
          ret.push_back(a.first); // original got this with push_front
          return ret;
        }
      );
    }

    void updateChildBasedAttributes(ChangeCell &change) {
      if(!this->initialized) {
        this->initialized = true;
        parser->updateChildBasedAttributes(change);
      }

      change.orWith(setParseNull(std::set<std::vector<A> >({
          std::vector<A>()
      })));
    }
};

template<class T, class A, class B, class F> class Red: public Parser<T, B> {
  public:
    Red(Parser<T, A> *parser, const F &f)
      : Parser<T, B>(false, false), parser(parser), f(f) {
      assert(parser);
    }

  private:
    Parser<T, A> *parser;
    const F &f;

  protected:
    Parser<T, B> *internalDerive(T t) {
      return new Red(parser->derive(t), f);
    }

  public:
    Stream<B> *parseFull(Stream<T> *input) {
      return new (class _: public Stream<B> {
        private:
          Stream<A> *src;
          const F &f;

        public:
          _(Stream<A> *src, const F &f): src(src), f(f) { }
          B head() { return f(src->head()); }
          _ *tail() { return new _(src->tail(), f); }
          bool isEmpty() { return src->isEmpty(); }
      }) (parser->parseFull(input), f);
    }

    Stream<std::pair<B, Stream<T> *> > *parse(Stream<T> *input) {
      return new (class _: public Stream<std::pair<B, Stream<T> *> > {
        private:
          Stream<std::pair<A, Stream<T> *> > *src;
          const F &f;

        public:
          _(Stream<std::pair<A, Stream<T> *> > *src, const F &f): src(src), f(f) { }
          std::pair<B, Stream<T> *> head() {
            auto h = src->head();
            return std::pair<B, Stream<T> *>(f(h.first), h.second);
          }
          _ *tail() { return new _(src->tail(), f); }
          bool isEmpty() { return src->isEmpty(); }
      }) (parser->parse(input), f);
    }

  protected:
    void updateChildBasedAttributes(ChangeCell &change) {
      if(!this->initialized) {
        this->initialized = true;
        parser->updateChildBasedAttributes(change);
      }
      
      std::set<A> a = parser->parseNull();
      std::set<B> b;
      for(auto i = a.begin(); i != a.end(); ++i) {
        b.insert(f(*i));
      }

      //for(auto i = b.begin(); i != b.end(); ++i) {
      //  std::cout << "setting: " << *i << std::endl;
      //}
      change.orWith(setParseNull(b));
      change.orWith(setIsEmpty(parser->isEmpty()));
      change.orWith(setIsNullable(parser->isNullable()));
    }
};

#if 0

object TestParser3 {

  lazy val EX : Parser[Char,Char] = new EqT[Char] ('x')
  
  lazy val EXL : Parser[Char,List[Char]] = 
    rule(EX ~ EXL) ==> { case x ~ xl => x :: xl } ||
    (new Eps(Stream.cons(List(),Stream.empty)))


  lazy val LX : Parser[Char,Char] = new EqT[Char] ('x')
  
  lazy val LXL : Parser[Char,List[Char]] = 
    rule(LXL ~ LX) ==> { case xl ~ x => x :: xl } ||
    rule(LX) ==> { case x => List(x) }


  lazy val LEX : Parser[Char,Char] = 
    new EqT[Char] ('x') 
  
  lazy val LEXL : Parser[Char,List[Char]] = 
    rule(LEXL ~ LEX) ==> { case xl ~ x => x :: xl } ||
    (new Eps(Stream.cons(List(),Stream.empty)))


  lazy val LPAR : Parser[Char,Char] = new EqT[Char] ('(')

  lazy val RPAR : Parser[Char,Char] = new EqT[Char] (')')
  
  lazy val PAR_LEXL : Parser[Char,List[Char]] = 
    rule(LPAR ~ LEXL ~ RPAR) ==> { case '(' ~ lexl ~ ')' => lexl } 
  

  abstract class SExp
  case class SXList(list : List[SExp]) extends SExp
  case class SXCons(head : SExp, tail : SExp) extends SExp
  case class SXSym(sym : Char) extends SExp
  case object SXNil extends SExp

  lazy val SX : Parser[Char,SExp] = 
    rule(LPAR ~ SXL ~ RPAR) ==> { case '(' ~ sxlist ~ ')' => sxlist } ||
    // rule(LPAR ~ RPAR) ==> { case '(' ~ ')' => SXNil } ||
    rule(S) ==> { case c => SXSym(c) }
  
  /*
  lazy val SXList : Parser[Char,SExp] = 
    rule(SX ~ SXList) ==> { case sx ~ sxlist => SXCons(sx,sxlist).asInstanceOf[SExp] } ||
    rule(Epsilon[Char]) ==> { case () => SXNil.asInstanceOf[SExp] }
    // rule(SX) ==> { case sx => sx.asInstanceOf[SExp] } 
  */
  lazy val SXL : Parser[Char,SExp] =
    rule(SX *) ==> { case sxlist => SXList(sxlist).asInstanceOf[SExp] }


  abstract class Exp
  case object One extends Exp
  case class Sum(e1 : Exp, e2 : Exp) extends Exp

  lazy val EXP : Parser[Char,Exp] = 
    rule(X) ==> { case x => One.asInstanceOf[Exp] } ||
    rule(EXP ~ S ~ EXP) ==> { case e1 ~ s ~ e2 => Sum(e1,e2).asInstanceOf[Exp] } ||
    rule(EXP ~ S ~ X) ==> { case e1 ~ s ~ e2 => Sum(e1,One).asInstanceOf[Exp] } ||
    rule(X ~ S ~ EXP) ==> { case e1 ~ s ~ e2 => Sum(One,e2).asInstanceOf[Exp] } ||
    rule(X) ==> { case x => One.asInstanceOf[Exp] } ||
    rule(EXP) ==> { case e => e } ||
    rule(Epsilon[Char]) ==> { case () => One } 
  


  // A benchmark construct:
  def benchmark (body : => Unit) : Long = {
    val start = java.util.Calendar.getInstance().getTimeInMillis()
    body
    val end = java.util.Calendar.getInstance().getTimeInMillis()
    end - start
  }

  def main (args : Array[String]) {

    val in = Stream.fromIterator("xxxxx".elements)

    val parses = XL.parse(in)

    println(parses.head)

    val parses2 = EXL.parse(in)

    println("parses2.head = " + parses2.head)

    println("parses2.tail.head = " + parses2.tail.head)

    println("parses2.tail.tail.head = " + parses2.tail.tail.head)



    val parses3 = EXL.parse(in)

    println(parses3.head)


    val parses4 = LEXL.parse(in)

    println(parses4.head)


    val in2 = Stream.fromIterator("(xxxx)".elements)

    val parses5 = PAR_LEXL.parseFull(in2)

    println(parses5.head)




    val xin = Stream.fromIterator("xsxsxsxsx".elements)
    
    var xparse1 = EXP.parseFull(xin)
    println("xparse1: " + xparse1)



    val sin = Stream.fromIterator("(sss(sss(s)(s)sss)ss(s))".elements)

    var sparse1 = SX.parseFull(sin)
    println(sparse1)

    val strings = Array("(ssss()ss()s()ss(sss(s)(s)sss)ss(s))" ,
                        "(ss()ss()ssssssssssssssssssssssssssssss()s(sss(s)(s)sss)ss(s))" ,
                        "(ss(())ss()ss()s((s)(s)sss)ss(s))" ,
                        "(ss((s))ss()ss()s((s)(s)sss)ss(s)(s)(s))") ;

    val trials = List(9,19,117,978,9171,118170,518170)

    val rng = new java.util.Random(10)

    for (trial <- trials) {
      var sexpNs = ((for (i <- (1 to trial).toList) yield {
        val i = rng.nextInt() ;
        val s = strings((if (i < 0) -i else i) % strings.length)
        s
      }).mkString(""))  ;
        
      // sexpNs = "(" + sexpNs + ")"

      // println("sexpNs: " + sexpNs)
      
      var input : Stream[Char] = Stream.fromIterator(sexpNs.elements)

      // println(input)
      
      var sparse2 : Stream[SExp] = null

      var count = 0 

      val time = benchmark {
        while (!input.isEmpty) {
          SX.parse(input) match {
            case Stream((tree,rest)) => {
              count = count + 1
              input = rest
            }
          } 
        }
      }

      // println("sparse2: " + sparse2)
      
      println("count: " + count)
      println("sexp"+trial+"s.length: " + sexpNs.length)
      println("time: " + time)
    }

    
    ()
  }
}
#endif

#endif
