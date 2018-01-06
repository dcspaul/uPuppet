
class base {
    file { "/tmp/multisubtest": content => "base", mode => "0644" }
}

class sub1 inherits base {
    File["/tmp/multisubtest"] { mode => "0755" }
}

class sub2 inherits base {
    File["/tmp/multisubtest"] { content => sub2 }
}

class main {
include sub1
include sub2
}
