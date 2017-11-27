#@Features: in
# This file tests the "in" operator (not for types)

# If left operatnd is a string
class in_string {

  # Test String Right
  if "test" in "thewordtestisinthisstring" {
    file { "/tmp/tst": content => "none" }
  }

  # Test case sensitivity
  if "TEST" in "thewordtestisinthisstring" {
    file { "/tmp/tst1": content => "none" }
  }

  if "test" in "thewordTESTisinthisstring" {
    file { "/tmp/tst2": content => "none" }
  }

  # Negative test
  if "test" in "notinthisstring" {
    file {"/this/file/is/wrong": ensure => "file" }
  }

  # Test Array Right
  $a = ["true", "moot", "foo"]
  $b = ["foo", "bar", "TEST" ]
  $c = ["BAR", "FOOBAR", "fOo", "TeSt", "Item", "VoId", "NotMe" ]
  $d = ["aoihjaoi", "hiajhpaekpa", "ahipejpamb", "aoihpak" ]

  if "true" in $a {
    file { "/tmp/tst3": content => "none" }
  }

  # Case test
  if "test" in $b {
    file { "/tmp/tst4": content => "none" }
  }

  if "test" in $c {
    file { "/tmp/tst5": content => "none" }
  }

  # Negative test
  if "notinlist" in $d {
    file { "/this/file/is/wrong2": ensure => "file" }
  }

  # Test Hash Right
  $e = {"true" =>  1, "moot" => 2, "foo" => 3 }
  $f = {"foo" => 3, "bar" => 10, "TEST" => 15}
  $g = {"BAR" => 0, "FOOBAR" => 1, "fOo" => 2, "TeSt" => 3, "Item" => 4, "VoId" => 5, "NotMe" => 6 }
  $h = {"phiea" => "test", "notme" => "TesT", "notmeeither" => "tESt", "none" => "TeSt"}

  if "true" in $e {
    file { "/tmp/tst6": content => "none" }
  }

  # Case test
  if "test" in $f {
    file { "/tmp/tst7": content => "none" }
  }

  if "test" in $g {
    file { "/tmp/tst8": content => "none" }
  }

  # Negative test (not in key test)
  if "test" in $h {
    file { "/this/file/is/wrong3": ensure => "file" }
  }
}

# Left operand is regex
class in_regex {
 
  # Test String Right
  if /.*test.*/ in "thewordtestisinthisstring" {
    file { "/tmp2/tst": content => "none" }
  }

  # Negative test
  if /test/ in "notinthisstring" {
    file {"/this2/file/is/wrong": ensure => "file" }
  }

  # Test Array Right
  $a = ["true", "moot", "foo"]
  $d = ["aoihjaoi", "hiajhpaekpa", "ahipejpamb", "aoihpak" ]

  if /(m|t)[or]ot/ in $a {
    file { "/tmp2/tst3": content => "none" }
  }

  # Negative test
  if /^wasdwasd.*$/ in $d {
    file { "/this2/file/is/wrong2": ensure => "file" }
  }

  # Test Hash Right
  $b = {"true" => 1, "moot" => 2, "foo" => 3 }
  $c = {"randm" => "test", "notme" =>  "TesT", "notmeeither" => "tESt", "none" => "TeSt"}

  if /^(t|f)[ro]o$/ in $b {
    file { "/tmp2/tst6": content => "none" }
  }

  # Negative test (not in key test)
  if /[tT](e|E)[sS]t$/ in $c {
    file { "/this2/file/is/wrong3": ensure => "file" }
  } 
}

class odd_behaviour {
  if { "hash" => "foo" } in { "hash" => "foo" } {
    file { "/weird/1": ensure => "file" }
  }

  if 4 in 8 {
    file { "/weird/2": ensure => "file" }
  }

  if 5 in { 5 => "test", "7" => "test2", "8" => "test3", 7 => "test4" } {
    file { "/weird/3": ensure => "file" }
  }

  if {"t" => "est"} in {{"t" => "est"} => "sure" } {
    file { "/weird/4": ensure => "file" }
  }

  if {/no_way/ => "est"} in {{/no_way/ => "est"} => "sure" } {
    file { "/weird/5": ensure => "file" }
  }
}

class main {

include in_string
include in_regex
include odd_behaviour
}

