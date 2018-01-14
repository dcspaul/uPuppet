# @FEATURES classes, class_overriding
#% {\em Code in the derived class is given special permission to override any resource
#% attributes that were set in the base class}
#% Compare with figure \ref{exfig:example17}. You can remove values by setting them to {\tt undef}.

class main inherits main::parent {
	File["file"] {
		owner => "alice",
		source => undef
	}
}

class main::parent {
	file { "file":
		owner => "bob",
		source => "the source"
	}
}
