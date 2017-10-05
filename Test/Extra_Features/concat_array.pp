#@Features: array_concatenation, negative indeces
# This test checkes that concatenating arrays works as intended with other arrays.

$var=["/tmp/file1","/tmp/file2"]
$var2 = ["/tmp/file5","/tmp/file6"]

class arraytest {
    $var = $var + ["/tmp/file3", "/tmp/file4"]
    file {
        $var[0]:
            content => "test"
    }
    file {
        $var[1]:
            content => "test"
    }
    file {
        $var[2]:
            content => "test"
    }
    file {
        $var[3]:
            content => "test"
    }

    $var2 = $var2 + [["/tmp/file7"]]
    file {
        $var2[0]:
            content => "test"
    }
    file {
        $var2[-2]:
            content => "test"
    }
    file {
        $var2[2][-1]:
            content => "test"
    }
}

class main {

include arraytest

}
