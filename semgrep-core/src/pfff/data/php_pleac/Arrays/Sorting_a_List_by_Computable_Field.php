# **********************************************************************
# Sorting a List by Computable Field
# **********************************************************************
<?php
function pleac_Sorting_a_List_by_Computable_Field() {
// Tasks in this section would typically use the PHP 'usort' family of functions
// which are used with a comparator function so as to perform custom comparisions.
// A significant difference from the Perl examples is that these functions are
// inplace sorters, so it is the original array that is modified. Where this must
// be prevented a copy of the array can be made and sorted

function comparator($left, $right)
{
  ; // Compare '$left' with '$right' returning result
}

// ------------

$ordered = array_slice($unordered);
usort($ordered, 'comparator');

// ----------------------------

// The Perl example looks like it is creating a hash using computed values as the key,
// array values as the value, sorting on the computed key, then extracting the sorted
// values and placing them back into an array

function compute($value)
{
  ; // Return computed value utilising '$value'
}

// ------------

// Original numerically-indexed array [sample data used]
$unordered = array(5, 3, 7, 1, 4, 2, 6);

// Create hash using 'compute' function to generate the keys. This example assumes that
// each value in the '$unordered' array is used in generating the corresponding '$key'
foreach($unordered as $value)
{
  $precomputed[compute($value)] = $value;
}

// Copy the hash, and sort it by key
$ordered_precomputed = array_slice($precomputed, 0); ksort($ordered_precomputed);

// Extract the values of the hash in current order placing them in a new numerically-indexed
// array
$ordered = array_values($ordered_precomputed);

// ----------------------------

// As above, except uses 'array_update' and 'accum' to help create hash

function array_update($arr, $lambda, $updarr)
{
  foreach($arr as $key) $lambda($updarr, $key);
  return $updarr;
}

function accum(&$arr, $value)
{
  $arr[compute($value)] = $value;
}

// ------------

function compute($value)
{
  ; // Return computed value utilising '$value'
}

// ------------

// Original numerically-indexed array [sample data used]
$unordered = array(5, 3, 7, 1, 4, 2, 6);

// Create hash
$precomputed = array_update($unordered, 'accum', array());

// Copy the hash, and sort it by key
$ordered_precomputed = array_slice($precomputed, 0); ksort($ordered_precomputed);

// Extract the values of the hash in current order placing them in a new numerically-indexed
// array
$ordered = array_values($ordered_precomputed);

// ----------------------------

class Employee
{
  public $name, $age, $ssn, $salary;

  public function __construct($name, $age, $ssn, $salary)
  {
    $this->name = $name;
    $this->age = $age;
    $this->ssn = $ssn;
    $this->salary = $salary;
  }
}

// ------------

$employees = array(
  new Employee('sdf', 27, 12345, 47000),
  new Employee('ajb', 32, 12376, 51000),
  new Employee('dgh', 31, 12355, 45000));

// ------------

$ordered = array_slice($employees, 0);
usort($ordered, create_function('$left, $right', 'return $left->name > $right->name;'));

// ------------

$sorted_employees = array_slice($employees, 0);
usort($sorted_employees, create_function('$left, $right', 'return $left->name > $right->name;'));

$bonus = array(12376 => 5000, 12345 => 6000, 12355 => 0);

foreach($sorted_employees as $employee)
{
  echo "{$employee->name} earns \${$employee->salary}\n";
}

foreach($sorted_employees as $employee)
{
  if (($amount = $bonus[$employee->ssn]))
    echo "{$employee->name} got a bonus of: \${$amount}\n";
}

// ------------

$sorted = array_slice($employees, 0);
usort($sorted, create_function('$left, $right', 'return $left->name > $right->name || $left->age != $right->age;'));

// ----------------------------

// PHP offers a swag of POSIX functions for obtaining user information [i.e. they all read
// the '/etc/passwd' file for the relevant infroamtion], and it is these that should rightly
// be used for this purpose. However, since the intent of this section is to illustrate array
// manipulation, these functions won't be used. Instead a custom function mimicing Perl's
// 'getpwent' function will be implemented so the code presented here can more faithfully match
// the Perl code

function get_pw_entries()
{
  function normal_users_only($e)
  {
    $entry = split(':', $e); return $entry[2] > 100 && $entry[2] < 32768;
  }

  foreach(array_filter(file('/etc/passwd'), 'normal_users_only') as $entry)
    $users[] = split(':', trim($entry));

  return $users;
}

// ------------

$users = get_pw_entries();

usort($users, create_function('$left, $right', 'return $left[0] > $right[0];'));
foreach($users as $user) echo "{$user[0]}\n";

// ----------------------------

$names = array('sdf', 'ajb', 'dgh');

$sorted = array_slice($names, 0);
usort($sorted, create_function('$left, $right', 'return substr($left, 1, 1) > substr($right, 1, 1);'));

// ------------

$strings = array('bbb', 'aa', 'c');

$sorted = array_slice($strings, 0);
usort($sorted, create_function('$left, $right', 'return strlen($left) > strlen($right);'));

// ----------------------------

function array_update($arr, $lambda, $updarr)
{
  foreach($arr as $key) $lambda($updarr, $key);
  return $updarr;
}

function accum(&$arr, $value)
{
  $arr[strlen($value)] = $value;
}

// ----

$strings = array('bbb', 'aa', 'c');

$temp = array_update($strings, 'accum', array());
ksort($temp);
$sorted = array_values($temp);

// ----------------------------

function array_update($arr, $lambda, $updarr)
{
  foreach($arr as $key) $lambda($updarr, $key);
  return $updarr;
}

function accum(&$arr, $value)
{
  if (preg_match('/(\d+)/', $value, $matches))
    $arr[$matches[1]] = $value;
}

// ----

$fields = array('b1b2b', 'a4a', 'c9', 'ddd', 'a');

$temp = array_update($fields, 'accum', array());
ksort($temp);
$sorted_fields = array_values($temp);

}
?>
