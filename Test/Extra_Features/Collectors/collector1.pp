#Extra_Features/Collectors/collector1.pp 
#@UNSUPPORTED
# Nodes include defined resources after all other statements, but before resource collectors
# Define after collector

define d (
  $backupArg = false 
  ) {
  file {'from_define':
    backup => $backupArg,
    path => $path,
  }    
}

node default {
  $backup=true
  File <| path == '/new' |> {
    backup => true
  }
  
  # Here $path is undef, therefore it does not pass forward
  d { "defined_res":
    backupArg => $backup,
  }

  $path = '/new'

  # At this point the defined resources are added with the prevously collected arguments
  # At this point the collector runs with the backup variable
}
