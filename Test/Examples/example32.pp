# @FEATURES classes, class_overriding
# @EXPECT_FAIL
#% References in an including class cannot extend resources in the included class.

class main {
	File["file"] { mode => 0644 }
	include main::sub
}

class main::sub {
	file { "file": owner => "alice" }
}
