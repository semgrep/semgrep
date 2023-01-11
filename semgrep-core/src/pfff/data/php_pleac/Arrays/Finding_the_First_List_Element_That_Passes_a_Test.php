# **********************************************************************
# Finding the First List Element That Passes a Test
# **********************************************************************
<?php
function pleac_Finding_the_First_List_Element_That_Passes_a_Test() {
// This section illustrates various 'find first' techniques. The Perl examples all use an
// explicit loop and condition testing [also repeated here]. This is the simplest, and
// [potentially] most efficient approach because the search can be terminated as soon as a
// match is found. However, it is worth mentioning a few alternatives:
// * 'array_search' performs a 'find first' using the element value rather than a condition
//    check, so isn't really applicable here
// * 'array_filter', whilst using a condition check, actually performs a 'find all', though
//   all but the first returned element can be discarded. This approach is actually less error
//   prone than using a loop, but the disadvantage is that each element is visited: there is no
//   means of terminating the search once a match has been found. It would be nice if this
//   function were to have a third parameter, a Boolean flag indicating whether to traverse
//   the whole array, or quit after first match [next version, maybe :) ?]

$found = FALSE;

foreach($array as $item)
{
  // Not found - skip to next item
  if (!$criterion) continue;

  // Found - save and leave
  $match = $item;
  $found = TRUE;
  break;
}

if ($found)
{
  ; // do something with $match
}
else
{
  ; // not found
}

// ------------

function predicate($element)
{
  if (criterion) return TRUE;
  return FALSE;
}

$match = array_slice(array_filter($array, 'predicate'), 0, 1);

if ($match)
{
  ; // do something with $match[0]
}
else
{
  ; // $match is empty - not found
}

// ----------------------------

class Employee
{
  public $name, $age, $ssn, $salary;

  public function __construct($name, $age, $ssn, $salary, $category)
  {
    $this->name = $name;
    $this->age = $age;
    $this->ssn = $ssn;
    $this->salary = $salary;
    $this->category = $category;
  }
}

// ------------

$employees = array(
  new Employee('sdf', 27, 12345, 47000, 'Engineer'),
  new Employee('ajb', 32, 12376, 51000, 'Programmer'),
  new Employee('dgh', 31, 12355, 45000, 'Engineer'));

// ------------

function array_update($arr, $lambda, $updarr)
{
  foreach($arr as $key) $lambda($updarr, $key);
  return $updarr;
}

function highest_salaried_engineer(&$arr, $employee)
{
  static $highest_salary = 0;

  if ($employee->category == 'Engineer')
  {
    if ($employee->salary > $highest_salary)
    {
      $highest_salary = $employee->salary;
      $arr[0] = $employee;
    }
  }
}

// ------------

// 'array_update' custom function is modelled on 'array_reduce' except that it allows the
// return of an array, contents and length of which are entirely dependant on what the
// callback function does. Here, it is logically working in a 'find first' capacity
$highest_salaried_engineer = array_update($employees, 'highest_salaried_engineer', array());

echo 'Highest paid engineer is: ' . $highest_salaried_engineer[0]->name . "\n";

}
?>
