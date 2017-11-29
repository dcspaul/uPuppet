#@UNSUPPORTED
#@Features: nested_hash, mixed_type_hash
# This file tests nested hash functionality and mixed type hash functionality

class odd_hash_syntax {


  $hash1 = { 5 => true, "test" => false,
    "nest" => {
       "yes!" => true,
        "no!" => false,
         1000 => { 10 => true, -10 => false,
                   "nest3" => {
                     ["yes", "this", "is", "fine"]  => true,
                     *["tst", "tst2", "tst3"] => false
                   }
         }
      }
  }


  # Dereferencing hash right after initialization (Useless but correct)
  if {/no_way/ => true}[/no_way/] {
    file { "/weird": ensure => "file" }
  }

  # Comma separated list as key = True
  if $hash1["nest"][1000]["nest3"]["yes","this","is","fine"] {
    file { "/weird1": ensure => "file" }
  }

  # List as Key = True
  if $hash1["nest"][1000]["nest3"][["yes","this","is","fine"]] {
    file { "/weird11": ensure => "file" }
  }

  # List element as Key = False 
  if $hash1["nest"][1000]["nest3"]["yes"] {
    file { "/weird2": ensure => "file" }
  }

  # Splat element = True
  unless $hash1["nest"][1000]["nest3"]["tst"] {
    file { "/weird3": ensure => "file" }
  }
}

class main {
  include odd_hash_syntax
}
