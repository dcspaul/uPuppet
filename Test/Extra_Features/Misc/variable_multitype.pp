# A variable can be multiple times based on conditions

define test ($path, $flag) {
  if $flag {
    $content = "foo"
  } else {
    $content = 10
  }

  file { $path:
    content => $content
  } 
}

node default {
  test {"res1": path => "test", flag => true }
  test {"res2": path => "test2", flag => false }
}
