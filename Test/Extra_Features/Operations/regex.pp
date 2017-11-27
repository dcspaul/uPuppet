#@Features: match, non_match
# This file tests regex operators (not type matching)

class regex {
  $regex = /.*test$/

  if "wasd " =~ /wasd[^\/]/ {
    file { "/tmp/tst": content => "none" }
  }

  if "test\nme now" =~ /test
me now/ {
    file { "/tmp/tst2": content => "none" }
  }

  if "test; test test" !~ "test[^; $]" {
    file { "/tmp/tst3": content => "none" }
  }
  
  unless "piabpainapvkontest" !~ $regex {
    file { "/tmp/tst4": content => "none" }
  }
}

class main {

include regex

}
