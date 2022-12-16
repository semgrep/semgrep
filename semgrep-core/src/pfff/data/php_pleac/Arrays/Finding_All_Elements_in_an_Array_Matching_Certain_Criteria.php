# **********************************************************************
# Finding All Elements in an Array Matching Certain Criteria
# **********************************************************************
<?php
function pleac_Finding_All_Elements_in_an_Array_Matching_Certain_Criteria() {
// PHP implements 'grep' functionality [as embodied in the current section] in the 'array_filter'
// function

function predicate($element)
{
  if (criterion) return TRUE;
  return FALSE;
}

$matching = array_filter($list, 'predicate');

// ------------

$bigs = array_filter($nums, create_function('$n', 'return $n > 1000000;'));

// ------------

function is_pig($user)
{
  $user_details = preg_split('/(\s)+/', $user);
  // Assuming field 5 is the resource being compared
  return $user_details[5] > 1e7;
}

$pigs = array_filter(array_slice(preg_split('/\n/', `who -u`), 0, -1), 'is_pig');

// ------------

$matching = array_filter(array_slice(preg_split('/\n/', `who`), 0, -1),
                         create_function('$user', 'return preg_match(\'/^gnat /\', $user);'));

// ------------

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

$engineers = array_filter($employees,
                          create_function('$employee', 'return $employee->category == "Engineer";'));

}
?>
