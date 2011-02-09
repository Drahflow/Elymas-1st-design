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
    Main();
    void parse(const std::string &input);

  private:
    Stream<char> *inStream;
    std::shared_ptr<Parser<char, TreeNode *> > parser;

    Grammar grammar;
    SymbolTableGlobal symbolTable;

    void (*compile(NodeStatement *))(Main *);
};

#endif
