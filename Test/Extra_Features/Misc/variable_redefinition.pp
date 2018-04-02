#@EXPECT_FAIL
# Variable Reassignment is not permitted

node default {
  $test = "test"
  file { $test:
    ensure => file,
  }
  $test = "test2"
  file { $test:
    ensure => file,
  }
  $test = 123
  file { "test3":
    mode => $test,
  }
}
