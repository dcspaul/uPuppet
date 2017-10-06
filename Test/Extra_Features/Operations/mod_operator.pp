#@Features: modulo
# This test checkes that the modulo operator is recognized and functional


class modtest {
    $var  = 18 % 3
    $var2 = 0.15 * 0.5
    file {
        "/tmp/mod.txt":
            content => $var
    }
    file {
        "/tmp/mod2.txt":
            content => $var2
    }
}

class main {

include modtest

}