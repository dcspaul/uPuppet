# @FEATURES classes, class_nesting
# @EXPECT_FAIL
#% Stupid class tricks: multiple definitions of a class from different nesting levels are OK, 
#% ergo multiple definitions of variables in the same class are forbidden

class main {
  include a
  include a::b
  user { $a::x : }
  user { $a::b::x : }
}

class a {
  $x = "a"
  class b {
    $x = "b"
  }
}

class a::b {
  $x = "ab"
}
