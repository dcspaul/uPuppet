#@UNSUPPORTED
# Default Values test, semicolon hash splitter
node default {
    file {
        default:
            owner => root,
            ensure => file,
        ;
        '/bin/test_file/':
            content => "new contents"
        ;
    }
}
