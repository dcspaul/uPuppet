#@Features: splat, negation, not
# This file tests unary operators in the Puppet Language 

class unary {
  $var=["/tmp/file1","/tmp/file2"]
  $dummy_var = ["/tmp/file2", "/tmp/file3"]
  $neg_10 = -10
  if !($neg_10 != -(5 + 10 - 5)) {
    file {
      *$var:
        content => "foo"
    }
  }
  case "/tmp/file1" {
    $var:  { file { "/tmp/array": content => "bar" } }
    *$dummy_var: { file {"/tmp/nope": content => bar } }
    *$var: { file {"/tmp/itemInArray": content => "bar" } }
  }
  
  $tmp = "/tmp/file1" ? {
    *$dummy_var => "/select/itemInArray",
    $var  => "/select/array",
    *$var => "/select/correct"
  }

  file { $tmp: content => "correct file" }  
}

class main {

include unary

}
