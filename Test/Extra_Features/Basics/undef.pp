#@FEATURES: undef
# This checks whether a resource is created if all parameters are undefined

class tst {
  file { "/test":
    content => $a,
    owner  => undef,
  }
}

class main {
  include tst
}
