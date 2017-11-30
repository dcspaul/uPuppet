#@Features trailing_comma

class param_test ($test = "UNDEF",) {
  file { $test: }
}

class main {
  file { "test1":
    content => "none",
    ensure => "file",
  }

  $hash = {
    "next_file" => "test2",
    "tst" => "none",
  }
  
  $array = ["none", "none2", "test3",]

  file { $hash["next_file"]: }
  file { $array[2]: }

  $file = true ? {
    false => "none",
    true => "test4",
  }

  file { $file: }
  class { param_test: test => "test5" }
  include param_test
}
