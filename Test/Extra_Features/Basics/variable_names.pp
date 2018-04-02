#@EXPECT_FAIL
#@UNSUPPORTED
# Variable names should start with a lowercase letter

node default {
  $TEST = "should_fail"
  file { $TEST:
    ensure => file,
    contents => $TEST,
  }
}
