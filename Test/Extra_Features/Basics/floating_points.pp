#@Features: floating point
# This test checkes that floating points are properly implemented with their binary operators


class modtest {
    $var  = 0.150000 / 0.050000
    $var2 = 0.15 * 0.5
    $var3 = 0.15 + 0.5
    $var4 = 0.15 - 0.5
    file {
        "/tmp/fp.txt":
            content => $var
    }
    file {
        "/tmp/fp2.txt":
            content => $var2
    }
    file {
        "/tmp/fp3.txt":
            content => $var3
    }
    file {
        "/tmp/fp4.txt":
            content => $var4
    }
}

class main {

include modtest

}