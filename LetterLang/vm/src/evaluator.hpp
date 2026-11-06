#ifndef EVAL_VM
#define EVAL_VM

#include <vector>
#include "./opcode.hpp"

namespace let {
  void eval(const std::vector<OpCode>& code);
}

#endif
