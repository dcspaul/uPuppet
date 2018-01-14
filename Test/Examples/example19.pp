# @FEATURES resources, resource_extension
#% Forward references to resources are supported.

class main {
  File["file"] { owner => "alice" }
  file { "file": mode => 0644 }
}
