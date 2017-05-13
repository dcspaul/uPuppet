# @caption: Class Parameters and Defined Resource Types
# @label: ex2
class c (                 #\plab{ex2:service2}
  $backupArg=false,
  $pathArg="/default",
  $modeArg=123 ) {

  file { "from_class":    #\plab{ex2:config2}
    backup => $backupArg,
    source => $pathArg,
    path => $path,
    mode => $modeArg
  }
}

define d (                #\plab{ex2:ddef}
  $backupArg=false,
  $pathArg="/default",
  $modeArg=123 ) {

  file { "from_define":   #\plab{ex2:config3}
    backup => $backupArg,
    source => $pathArg,
    path => $path,
    mode => $modeArg
  }
}

node default {

  $backup = true

  class { c:              #\plab{ex2:cdecl}
    backupArg => $backup,
    pathArg => $path 
  }

  d { "service3":         #\plab{ex2:dcall}
    backupArg => $backup,
    pathArg => $path
  }

  $path = "/path"
}
