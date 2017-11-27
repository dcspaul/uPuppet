#@FEATURES nodes
# @OPTIONS --strict_variables
#% If a name is not found in a class scope with an inheritance edge, lookup
#% proceeds into node scope.


class main () inherits main::parent {
user { "alice": comment => $comment }
}

class main::parent {
}


node default {
  $comment = "the comment"
  include main 
}
