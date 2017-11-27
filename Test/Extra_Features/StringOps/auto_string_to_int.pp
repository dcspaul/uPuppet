#@UNSUPPORTED
#@FEATURES auto_string_to_number_conversion
class main {
  $a = "10.10"
  $b = "10"
  $c = $a + $b
  $d = "$a" + "$b" + "$c"
  file { "/tmp/$c": ensure => "file"}
  file { "/tmp/$d": ensure => "file"}
  if true and "true" {
    file { "/tmp/booleanConv": ensure => "file"}
  }
}
