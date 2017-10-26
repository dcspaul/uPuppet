#@FEATURES in_string_variables
class main {

$value = false

file { "/tmp/falsevalues${value}": ensure => "file" }

$a = true
$b = "test$a"
$c = "test3$b $a"
$d = "test4${c}${b}${a}"
$e = 10
$f = "test$e"
$g = 10.1010
$h = "${g}test"

file { "/tmp/$a": ensure => "file"}
file { "/tmp/$b": ensure => "file"}
file { "/tmp/$c": ensure => "file"}
file { "/tmp/$d": ensure => "file"}
file { "/tmp/$f": ensure => "file"}
file { "/tmp/$h": ensure => "file"}
include testundef
include testconditionals
}

class testundef {
file { "/tmp/no$value": ensure => "file"}
}

class testconditionals {
$a = "wasd"
$b = "wasd"
$c = "bar"
if "$a" == "$b" {
    file { "/tmp/if": ensure => "file"}
}
unless "$a" != "$b" {
    file { "/tmp/unless": ensure => "file"}
}
case "foobar" {
    "$a": { file {"/tmp/wrong": ensure => "file"} }
    "foo$c": { file {"/tmp/right": ensure => "file"} }
}
$foo = "foobar"
$d = "bar"

file {
    "/tmp/test": content => $foo ? {
	"bar" => "wrong2",
	"foo$d" => "right2"
    }
}
}
