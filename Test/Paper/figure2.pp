# @caption: Resources and Classes
# @label: ex1
node default {
  $source = "/source"
  include service1
}

class service1 {

  $mode = 123                 # \plab{ex1:var1}

  include service2

  file { "config1":           # \plab{ex1:config1}
    path => "path1",
    source => $source,
    mode => $mode,
    checksum => $checksum,
    provider => $provider,
    recurse => $recurse
  }

  $checksum = md5             # \plab{ex1:var2}
}

class service2                # \plab{ex1:service2}
  inherits service3 {

  $recurse = true             # \plab{ex1:B}

  file { "config2":           # \plab{ex1:config2}
    path => "path2",
    source => $source,
    mode => $mode,
    checksum => $checksum,
    provider => $provider,
    recurse => $recurse
  }
}

class service3 {              # \plab{ex1:service3}

  $provider = posix

  file { "config3":           # \plab{ex1:config3}
    path => "path3",
    mode => $mode,
    checksum => $checksum,
    recurse => $recurse
  }
}
