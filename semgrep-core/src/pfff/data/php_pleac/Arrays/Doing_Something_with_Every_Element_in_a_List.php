# **********************************************************************
# Doing Something with Every Element in a List
# **********************************************************************
<?php
function pleac_Doing_Something_with_Every_Element_in_a_List() {
foreach ($list as $item) {
    // do something with $item
}

// Environment listing example

// PHP defines a superglobal $_ENV to provide access to environment
// variables.

// Beware, array assignment means copying in PHP. You need to use
// the reference operator to avoid copying. But we want a copy here.
$env = $_ENV;

// PHP can sort an array by key, so you don't need to get keys,
// and then sort.
ksort($env);

foreach ($env as $key => $value) {
    echo "{$key}={$value}\n";
}

// Literal translation of Perl example would be:
$keys = array_keys($_ENV);
sort($keys);
foreach ($keys as $key) {
    echo "{$key}={$_ENV[$key]}\n";
}

// This assumes that MAX_QUOTA is a named constant.
foreach ($all_users as $user) {
    $disk_space = get_usage($user);
    if ($disk_space > MAX_QUOTA) {
        complain($user);
    }
}

// You can't modify array's elements in-place.
$array = array(1, 2, 3);
$newarray = array();
foreach ($array as $item) {
    $newarray[] = $item - 1;
}
print_r($newarray);

// Before PHP 5, that is. You can precede the reference operator
// before $item to get reference instead of copy.
$array = array(1, 2, 3);
foreach ($array as &$item) {
    $item--;
}
print_r($array);

// TODO: explain the old each() and list() iteration construct.
// foreach is new in PHP 4, and there are subtle differences.

}
?>
