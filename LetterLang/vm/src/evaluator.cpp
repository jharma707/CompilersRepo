#include "./evaluator.hpp"
#include <type_traits>

#define READ_BYTE *ip++

void let::eval(const std::vector<OpCode>& code) {
  if (code.empty()) return;

  const OpCode *ip = &code[0];

  for (;;) {
    auto opcode = READ_BYTE;
    auto opcode_ordinal = static_cast<std::underlying_type_t<OpCode>>(opcode);
    log(opcode_ordinal);

    switch (opcode) {
      case OpCode::HALT: return;
    }
  }
}
