#@UNSUPPORTED
# Nodes include defined resources after all other statements, but before resource collectors
# Define after collector

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
