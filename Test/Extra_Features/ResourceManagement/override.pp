# overloading parameters

class bar {
  file { "/test":
    ensure => "file",
    content => "not this",
  }
  File["/test"] { owner => "root" }
}

class foo inherits bar {
  File["/test"] { content => "this" }
}

class main {
  include foo
}
