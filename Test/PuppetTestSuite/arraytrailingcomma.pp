#@UNSUPPORTED
class main {

file {
    ["/tmp/arraytrailingcomma1","/tmp/arraytrailingcomma2", ]: content => "tmp"
}

}