#@UNSUPPORTED
#@FEATURES=class_parameters, class_inheritance, resource_parameter_override, strongly_typed_variables
#This is an example of existing obscure syntax for declaring classes
#unlike the include keyword this allows passing class parameters

class a (
  String $path = "/tmp/defaultPath",
  Numeric $mode = 6777
  ) {
  file {
    "testFile":
      path => $path,
      mode => $mode
  }
}

class b (String $contents = 'DEFAULT') inherits a {
  File['testFile'] { content => $contents }
}

class main {
  class {'a':
    path => "/tmp/newPath",
    mode => 5777
  }
  class {'b':
    contents => "NEW"
  }
}
