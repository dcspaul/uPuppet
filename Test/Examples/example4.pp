# @FEATURES classes, class_overriding
# @EXPECT_FAIL
#% References in an included class cannot extend resources in the including class.
#% This is bit surprising. 
#% The error message terminology also seems a bit misleading.

class main {
	file { "file": owner => "alice" }
	include main::sub
}

class main::sub {
	File["file"] { mode => 0644 }
}
