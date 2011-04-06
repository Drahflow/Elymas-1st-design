#ifndef MAIN_H
#define MAIN_H

#include "parser.h"
#include "treenodes.h"
#include "grammar.h"
#include "symboltable.h"

#include <string>
#include <fstream>
#include <iostream>
#include <memory>

class Main {
  public:
    Main(bool debug);
    void parse(const std::string &input);

  private:
    Stream<char> *inStream;
    std::shared_ptr<Parser<char, TreeNode *> > parser;

    Grammar grammar;
    SymbolTableGlobal symbolTable;
    bool debug;

    void (*compile(NodeStatement *))(Main *);
};

#endif
