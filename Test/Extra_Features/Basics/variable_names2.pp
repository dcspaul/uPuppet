#@EXPECT_FAIL
# Variables cannot start with a number

node default {
  $1test = "should_fail"
  file { $1test:
    ensure => file,
    contents => $1test,
  }
}
