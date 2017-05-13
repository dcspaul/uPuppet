

class service1 {
  $mode = 123

  include service2

  file { "config1":
    path => "path1",
    source => $source,
    mode => $mode,
    checksum => $checksum,
    provider => $provider,
    recurse => $recurse
  }

  $checksum = md5
}

class service2 inherits service3 {
  $recurse = true

  file { "config2":
    path => "path2",
    source => $source,
    mode => $mode,
    checksum => $checksum,
    provider => $provider,
    recurse => $recurse
  }
}

class service3 {
  $provider = posix

  file { "config3":
    path => "path3",
    mode => $mode,
    checksum => $checksum,
    recurse => $recurse
  }
}

node default {
  $source = "/source"
  include service1
}
