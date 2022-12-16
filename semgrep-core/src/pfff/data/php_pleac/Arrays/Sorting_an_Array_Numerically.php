# **********************************************************************
# Sorting an Array Numerically
# **********************************************************************
<?php
function pleac_Sorting_an_Array_Numerically() {
// PHP offers a rich set of sorting functions. Key features:
// * Inplace sorts; the original array, not a a copy, is sorted
// * Separate functions exist for sorting [both ascending and descending order]:
//   - By value, assign new keys / indices [sort, rsort]
//   - By key   [ksort, krsort] (for non-numerically indexed arrays)
//   - By value [asort, arsort]
//   - As above, but using a user-defined comparator [i.e. callback function]
//     [usort, uasort, uksort]
//   - Natural order sort [natsort]
// * Significantly, if sorting digit-only elements, whether strings or numbers,
//   'natural order' [i.e. 1 before 10 before 100 (ascending)] is retained. If
//   the elements are alphanumeric e.g. 'z1', 'z10' then 'natsort' should be
//   used [note: beware of 'natsort' with negative numbers; prefer 'sort' or 'asort']

$unsorted = array(7, 12, -13, 2, 100, 5, 1, -2, 23, 3, 6, 4);

sort($unsorted);                 // -13, -2, 1, 2, 3, 4, 5, 6, 7, 12, 23, 100
rsort($unsorted);                // 100, 23, 12, 7, 6, 5, 4, 3, 2, 1, -2, -13

asort($unsorted);                // -13, -2, 1, 2, 3, 4, 5, 6, 7, 12, 23, 100
arsort($unsorted);               // 100, 23, 12, 7, 6, 5, 4, 3, 2, 1, -2, -13

natsort($unsorted);              // -2, -13, 1, 2, 3, 4, 5, 6, 7, 12, 23, 100

// ------------

function ascend($left, $right) { return $left > $right; }
function descend($left, $right) { return $left < $right; }

// ------------

usort($unsorted, 'ascend');      // -13, -2, 1, 2, 3, 4, 5, 6, 7, 12, 23, 100
usort($unsorted, 'descend');     // 100, 23, 12, 7, 6, 5, 4, 3, 2, 1, -2, -13

uasort($unsorted, 'ascend');     // -13, -2, 1, 2, 3, 4, 5, 6, 7, 12, 23, 100
uasort($unsorted, 'descend');    // 100, 23, 12, 7, 6, 5, 4, 3, 2, 1, -2, -13

// ----------------------------

function kill_process($pid)
{
  // Is 'killable' ?
  if (!posix_kill($pid, 0)) return;

  // Ok, so kill in two stages
  posix_kill($pid, 15); // SIGTERM
  sleep(1);
  posix_kill($pid, 9);  // SIGKILL
}

function pid($pentry)
{
  $p = preg_split('/\s/', trim($pentry));
  return $p[0];
}

$processes = array_map('pid', array_slice(preg_split('/\n/', `ps ax`), 1, -1));
sort($processes);

echo join(' ,', $processes) . "\n";

echo 'Enter a pid to kill: ';
if (($pid = trim(fgets(STDIN))))
  kill_process($pid);

}
?>
