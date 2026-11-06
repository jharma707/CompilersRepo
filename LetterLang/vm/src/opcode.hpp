#ifndef OP_CODE
#define OP_CODE

#include <cstdint>
#include <iostream>

#define log(value) std::cout << #value << " = " << +(value) << "\n"

namespace let {
  enum class OpCode : std::uint8_t {
    HALT = 0,
  };
}

#endif
