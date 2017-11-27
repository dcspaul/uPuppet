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
  if {/no_way/ => true}[/no way/] {
    file { "/test5": ensure => "file" }
  }

  # Dereferencing on list parameter = True
  if $hash1["nest"][1000]["nest3"]["yes","this","is","fine"] {
    file { "/weird1": ensure => "file" }
  }

  # Dereferencing on list item = True
  if $hash1["nest"][1000]["nest3"]["yes"] {
    file { "/weird1": ensure => "file" }
  }
}

class main {
  include odd_hash_syntax
}
