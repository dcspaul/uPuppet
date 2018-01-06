# @FEATURES resources, resource_extension
#% Variables can be defined and referenced.
#% Note that the variables do {\em not} appear explicitly in the catalog - but they can be 
#% substituted in resource titles and values and names, and used in other expressions
#% (for example, conditionals).

class main {
	$owner = "alice"
	$file = "file"
	file { "file": owner => $owner }
	File[$file] { mode => 0755 }
}
