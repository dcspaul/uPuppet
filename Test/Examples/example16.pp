# @FEATURES resources, resource_extension
#% Attributes can be added to a resource which has been previously defined
#% by using a reference to the resource.

class main {
  file { "file": owner => "alice" }
  File["file"] { mode => 0755 }
}

