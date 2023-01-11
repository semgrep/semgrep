# **********************************************************************
# Filtering Your Own Output
# **********************************************************************
<?php
function pleac_Filtering_Your_Own_Output() {
// Output buffering: Only display a certain number of lines of output.
class Head {
    function Head($lines=20) {
        $this->lines = $lines;
    }

    function filter($output, $mode) {
        $result = array();
        $newline = '';
        if (strlen($output) > 0 && $output[strlen($output) - 1] == "\n") {
            $newline = "\n";
            $output = substr($output, 0, -1);
        }
        foreach (explode("\n", $output) as $i => $line) {
            if ($this->lines > 0) {
                $this->lines--;
                $result[] = $line;
            }
        }
        return $result ? implode("\n", $result) . $newline : '';
    }
}

// Output buffering: Prepend line numbers to each line of output.
class Number {
    function Number() {
        $this->line_number = 0;
    }

    function filter($output, $mode) {
        $result = array();
        $newline = '';
        if (strlen($output) > 0 && $output[strlen($output) - 1] == "\n") {
            $newline = "\n";
            $output = substr($output, 0, -1);
        }
        foreach (explode("\n", $output) as $i => $line) {
            $this->line_number++;
            $result[] = $this->line_number . ': ' . $line;
        }
        return implode("\n", $result) . $newline;
    }
}

// Output buffering: Prepend "> " to each line of output.
class Quote {
    function Quote() {
    }

    function filter($output, $mode) {
        $result = array();
        $newline = '';
        if (strlen($output) > 0 && $output[strlen($output) - 1] == "\n") {
            $newline = "\n";
            $output = substr($output, 0, -1);
        }
        foreach (explode("\n", $output) as $i => $line) {
            $result[] = "> $line";
        }
        return implode("\n", $result) . $newline;
    }
}

// Use arrays as callbacks to register filter methods.
ob_start(array(new Head(100), 'filter'));
ob_start(array(new Number(), 'filter'));
ob_start(array(new Quote(), 'filter'));

// Act like /bin/cat.
while (!feof(STDIN)) {
    $line = fgets(STDIN);
    if ($line === false) break;
    echo $line;
}

// Should match number of calls to ob_start().
ob_end_flush();
ob_end_flush();
ob_end_flush();

}
?>
