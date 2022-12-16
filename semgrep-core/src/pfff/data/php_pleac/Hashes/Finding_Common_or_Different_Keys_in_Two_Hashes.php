# **********************************************************************
# Finding Common or Different Keys in Two Hashes
# **********************************************************************
<?php
function pleac_Finding_Common_or_Different_Keys_in_Two_Hashes() {
// PHP offers a number of array-based 'set operation' functions:
// * union:        array_merge
// * intersection: array_intersect and family
// * difference:   array_diff and family
// which may be used for this type of task

// Keys occurring in both hashes
$common = array_intersect_key($h1, $h2);

// Keys occurring in the first hash [left side], but not in the second hash
$this_not_that = array_diff_key($h1, $h2);

// ----------------------------

$food_colour = array('Apple' => 'red', 'Banana' => 'yellow',
                     'Lemon' => 'yellow', 'Carrot' => 'orange');

$citrus_colour = array('Lemon' => 'yellow', 'Orange' => 'orange', 'Lime' => 'green');

$non_citrus = array_diff_key($food_colour, $citrus_colour);

}
?>
