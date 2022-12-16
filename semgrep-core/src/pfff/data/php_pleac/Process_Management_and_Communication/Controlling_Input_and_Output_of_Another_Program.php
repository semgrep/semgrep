# **********************************************************************
# Controlling Input and Output of Another Program
# **********************************************************************
<?php
function pleac_Controlling_Input_and_Output_of_Another_Program() {
// Connect to input and output of a process.
$proc = proc_open($program,
                  array(0 => array('pipe', 'r'),
                        1 => array('pipe', 'w')),
                  $pipes);
if (is_resource($proc)) {
    fwrite($pipes[0], "here's your input\n");
    fclose($pipes[0]);
    echo stream_get_contents($pipes[1]);
    fclose($pipes[1]);
    $result_code = proc_close($proc);
    echo "$result_code\n";
}

// -----------------------------

$all = array();
$outlines = array();
$errlines = array();
exec("( $cmd | sed -e 's/^/stdout: /' ) 2>&1", $all);
foreach ($all as $line) {
    $pos = strpos($line, 'stdout: ');
    if ($pos !== false && $pos == 0) {
        $outlines[] = substr($line, 8);
    } else {
        $errlines[] = $line;
    }
}
print("STDOUT:\n");
print_r($outlines);
print("\n");
print("STDERR:\n");
print_r($errlines);
print("\n");

}
?>
