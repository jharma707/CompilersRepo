#include "./evaluator.hpp"

using namespace let;

int main(int argc, char **argv) {
  auto code = std::vector<OpCode>{
    OpCode::HALT
  };

  eval(code);
  return 0;
}
