#include "./evaluator.hpp"

#define READ_BYTE *ip++

void let::eval(const std::vector<OpCode>& code) {
  if (code.empty()) return;

  const OpCode *ip = &code[0];

  for (;;) {
    switch (READ_BYTE) {
      case OpCode::HALT: return;
    }
  }
}
