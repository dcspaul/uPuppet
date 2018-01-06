#@EXPECT_FAIL
# Only Subclasses can override (not necessarily nested scopes)

file { "/test2":
  ensure => directory,
  content => "wtf",
}

class main {
  File["/test2"] { content => "lol" }
}
