#@EXPECT_FAIL
#A defined resource cannot be called into the resource collector

define d (
  $backupArg = false,
  $pathArg = '/default',
  $modeArg = 123
  ) {
  file {'from_define':
    backup => $backupArg,
    source => $pathArg,
    path => $path,
    mode => $modeArg
  }    
}

node default {
  $backup=true
  d { "res":
    backupArg => true
  }

  d <| backupArg == true |> {
    backupArg => false
  }
}
