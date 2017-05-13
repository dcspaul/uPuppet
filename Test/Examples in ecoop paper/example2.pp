class c (
  $backupArg = false,
  $pathArg = "/default",
  $modeArg = 123 ) {

  file { "from_class":
    backup => $backupArg,
    source => $pathArg,
    path => $path,
    mode => $modeArg
  }
}

define d (
  $backupArg = false,
  $pathArg = "/default",
  $modeArg = 123 ) {

  file { "from_define":
    backup => $backupArg,
    source => $pathArg,
    path => $path,
    mode => $modeArg
  }
}

node default {

   $backup = true
    $path = "/path"
   
  class { c:
    backupArg => $backup,
    pathArg => $path 
  }

d { "service3":
      backupArg => $backup ,
      pathArg => $path}

}



