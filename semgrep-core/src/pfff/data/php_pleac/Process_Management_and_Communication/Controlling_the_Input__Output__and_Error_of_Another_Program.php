# **********************************************************************
# Controlling the Input, Output, and Error of Another Program
# **********************************************************************
<?php
function pleac_Controlling_the_Input__Output__and_Error_of_Another_Program() {
$proc = proc_open($cmd,
                  array(0 => array('pipe', 'r'),
                        1 => array('pipe', 'w'),
                        2 => array('pipe', 'w')),
                  $pipes);

if (is_resource($proc)) {
    // give end of file to kid, or feed him
    fclose($pipes[0]);

    // read till EOF
    $outlines = array();
    while (!feof($pipes[1])) {
        $line = fgets($pipes[1]);
        if ($line === false) break;
        $outlines[] = rtrim($line);
    }

    // XXX: block potential if massive
    $errlines = array();
    while (!feof($pipes[2])) {
        $line = fgets($pipes[2]);
        if ($line === false) break;
        $errlines[] = rtrim($line);
    }

    fclose($pipes[1]);
    fclose($pipes[2]);
    proc_close($proc);

    print("STDOUT:\n");
    print_r($outlines);
    print("\n");
    print("STDERR:\n");
    print_r($errlines);
    print("\n");
}

// -----------------------------

// cmd3sel - control all three of kids in, out, and error.
$cmd = "grep vt33 /none/such - /etc/termcap";
$proc = proc_open($cmd,
                  array(0 => array('pipe', 'r'),
                        1 => array('pipe', 'w'),
                        2 => array('pipe', 'w')),
                  $pipes);

if (is_resource($proc)) {
    fwrite($pipes[0], "This line has a vt33 lurking in it\n");
    fclose($pipes[0]);

    $readers = array($pipes[1], $pipes[2]);
    while (stream_select($read=$readers,
                         $write=null,
                         $except=null,
                         0, 200000) > 0) {
        foreach ($read as $stream) {
            $line = fgets($stream);
            if ($line !== false) {
                if ($stream === $pipes[1]) {
                    print "STDOUT: $line";
                } else {
                    print "STDERR: $line";
                }
            }
            if (feof($stream)) {
                $readers = array_diff($readers, array($stream));
            }
        }
    }

    fclose($pipes[1]);
    fclose($pipes[2]);
    proc_close($proc);
}

}
?>
