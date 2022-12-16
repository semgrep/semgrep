# **********************************************************************
# High-Resolution Timers
# **********************************************************************
<?php
function pleac_High_Resolution_Timers() {
// PHP 5 and above can use the built-in, 'microtime'. Crude implementation for ealier versions:
// function microtime() { $t = gettimeofday(); return (float) ($t['sec'] + $t['usec'] / 1000000.0); }

// ------------

$before = microtime();

$line = fgets(STDIN);

$elapsed = microtime() - $before;

printf("You took %.3f seconds\n", $elapsed);

// ------------

define(NUMBER_OF_TIMES, 100);
define(SIZE, 500);

for($i = 0; $i < NUMBER_OF_TIMES; $i++)
{
  $arr = array();
  for($j = 0; $j < SIZE; $j++) $arr[] = rand();

  $begin = microtime();
  sort($arr);
  $elapsed = microtime() - $begin;

  $total_time += $elapsed;
}

printf("On average, sorting %d random numbers takes %.5f seconds\n", SIZE, $total_time / (float) NUMBER_OF_TIMES);

}
?>
