#include "main.h"

#include "grammar.h"
#include "assembly.h"
#include "types.h"
#include "library.h"
#include "opcodes.h"

Main::Main(bool debug): debug(debug) {
  Library::addStandardTypes(&symbolTable);
  Library::addStandardFunctions(&symbolTable);
}

void Main::parse(const std::string &file) {
  std::ifstream input(file);
  if(!input.good()) {
    std::cerr << "Failed to open input" << std::endl;
  }

  inStream = new (class _: public Stream<char> {
    private:
      size_t offset;
      std::ifstream &input;

    public:
      _(size_t offset, std::ifstream &input): offset(offset), input(input) { }
      char head() { input.seekg(offset); return input.get(); }
      _ *tail() { return new _(offset + 1, input); }
      bool isEmpty() { input.seekg(offset); return !input.good(); }
  }) (0, input);

  parser = std::shared_ptr<Parser<char, TreeNode *> >(grammar.generateParser());
  auto whitespace = grammar.getWhitespace();

  while(!inStream->isEmpty()) {
    auto statements = parser->parse(inStream);

    if(statements->isEmpty()) {
      std::cerr << "no valid parse found at: ";
      for(int i = 0; i < 30 && !inStream->isEmpty(); ++i) {
        std::cerr << inStream->head();
        inStream = inStream->tail();
      }
      std::cerr << "..." << std::endl;
      break;
    }

    void (*exec)(Main *) = compile(
        dynamic_cast<NodeStatement *>(statements->head().first));
    if(exec) {
      exec(this);
    }

    inStream = statements->head().second;

    auto next = whitespace->parse(inStream);

    inStream = next->head().second;
  }

  if(debug) {
    std::cout << "Nodes created: " << TreeNode::creations << std::endl;
    std::cout << "Parsers created: " << Parser_creations << std::endl;
  }
}

void (*Main::compile(NodeStatement *statement))(Main *) {
  assert(statement);

  statement->rewriteDeclarations(&symbolTable);
  statement->resolveSymbols(&symbolTable);

  statement->assignUnresolvedTypes(Type::any);
  statement->rewriteFunctionApplications();

  if(debug) std::cout << statement->dump(0) << std::endl;

  Assembly assembly;
  statement->compile(assembly);
  assembly.add(opcode::retnear());
  return reinterpret_cast<void (*)(Main *)>(assembly.assemble());
}
