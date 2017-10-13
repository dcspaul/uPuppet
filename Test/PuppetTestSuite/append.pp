#@UNSUPPORTED
#@FEATURES array_concat, array_expansion

$var=["/tmp/file1","/tmp/file2"]

class arraytest {
    $var = $var + ["/tmp/file3", "/tmp/file4"]
    file {
        $var:
            content => "test"
    }
}

class main {

include arraytest

}
