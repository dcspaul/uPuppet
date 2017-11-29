#@Features: hex, bit_shifts
# This file tests the bit shift operators and hex values

class lshift {
  $decimal = 2
  $hex     = 0x00001

  file {
    "test1": content => $decimal << 2
  }
  
  file {
    "test2": content => $hex << 5
  }
}

class rshift {
  $decimal = -10
  $hex     = 0xFFF

  file {
    "test3": content => $decimal >> 2
  }
  
  file {
    "test4": content => $hex >> 2
  }
}
class main {

include lshift
include rshift
}
