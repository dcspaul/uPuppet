#@UNSUPPORTED
# Regex Selector evaluation is unsupported (with extra regex parameters)
node default {
  $os = "Ubuntu"
  $test = $os ? {
    /(?i-mx:ubuntu|debian)/        => "apache2",
    /(?i-mx:centos|fedora|redhat)/ => "httpd",
  }
  package { $test:
    ensure => installed
  }
}
