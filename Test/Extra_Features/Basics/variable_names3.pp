#@EXPECT_FAIL
# Variables cannot start with a number

node default {
  $_test = "should_fail"
  file { $_test:
    ensure => file,
    contents => $_test,
  }
}
