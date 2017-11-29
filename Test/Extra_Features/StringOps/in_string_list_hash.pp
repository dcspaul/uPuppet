#@UNSUPPORTED
#@FEATURES in_stirng_complex_types

class main {

  $list = [ "test1", "test2", "test3" ]
  $hash = { "foo" => 10, "bar" => "foobar" }
  
  file { "/tmp/$list": ensure => "file"}
  file { "/tmp/$hash": ensure => "file"}

}
