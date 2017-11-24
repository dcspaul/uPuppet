#@UNSUPPORTED
#@FEATURES auto_add_path
# Automatically Add 'path' to catalog if file is a directory

class main {
file { "/tmp/": ensure => "file"}
}
