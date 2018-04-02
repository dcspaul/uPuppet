#@UNSUPPORTED
# Defined resources are created before classes
# Define after collector


class main() {
  $backup=true
  File <| backup == true |> {
    backup => false
  }
  
  # Here $path is undef, therefore it does not pass forward
  d { "defined_res":
    backupArg => $backup,
    pathArg => $path
  }

  $path = '/path'

  # At this point the defined resources are added with the prevously collected arguments
  # At this point the collector runs with the backup variable
}

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
