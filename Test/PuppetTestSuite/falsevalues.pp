#@UNSUPPORTED
#@FEATURES in_string_variables
class main {

$value = false

file { "/tmp/falsevalues$value": ensure => "file" }
}
