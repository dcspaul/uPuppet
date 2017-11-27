#@Features: nested_hash, mixed_type_hash
# This file tests nested hash functionality and mixed type hash functionality

# If left operatnd is a string
class nested_hash {
  $hash1 = { 5 => true, "test" => false,
    "nest" => {
       "yes!" => true,
        "no!" => false,
         1000 => { 10 => true, -10 => false,
                   "nest3" => {
                     "yes"  => true,
                     "no"   => false
                   }
         }
      }
  }

  if $hash1[5] {
    file { "/test1": ensure => "file" }
  }

  if $hash1["nest"]["yes!"] {
    file { "/test2": ensure => "file" }
  }

  if $hash1["nest"][1000][10] {
    file { "/test3": ensure => "file" }
  }

  unless $hash1["nest"][1000]["nest3"]["yes"] {
    file { "/test4": ensure => "file" }
  }
}

class main {
include nested_hash
}

