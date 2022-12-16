# **********************************************************************
# Passing by Named Parameter
# **********************************************************************
<?php
function pleac_Passing_by_Named_Parameter() {
// PHP doesn't directly support named / keyword parameters, but these can be
// easily mimiced using a class of key / value pairs, and passing a variable
// number of arguments

class KeyedValue
{
  public $key, $value;
  public function __construct($key, $value) { $this->key = $key; $this->value = $value; }
}

function the_func()
{
  foreach (func_get_args() as $arg)
  {
    printf("Key: %10s|Value:%10s\n", $arg->key, $arg->value);
  }
}

// ----

the_func(new KeyedValue('name', 'Bob'),
         new KeyedValue('age', 36),
         new KeyedValue('income', 51000));

// ----------------------------

// Alternatively, an associative array of key / value pairs may be constructed.
// With the aid of the 'extract' built-in function, the key part of this array
// may be intsntiated to a variable name, thus more closely approximating the
// behaviour of true named parameters

function the_func($var_array)
{
  extract($var_array);

  if (isset($name)) printf("Name:   %s\n", $name);
  if (isset($age)) printf("Age:    %s\n", $age);
  if (isset($income)) printf("Income: %s\n", $income);
}

// ----

the_func(array('name' => 'Bob', 'age' => 36, 'income' => 51000));

// ----------------------------

class RaceTime
{
  public $time, $dim;
  public function __construct($time, $dim) { $this->time = $time; $this->dim = $dim; }
}

function the_func($var_array)
{
  extract($var_array);

  if (isset($start_time)) printf("start:  %d - %s\n", $start_time->time, $start_time->dim);
  if (isset($finish_time)) printf("finish: %d - %s\n", $finish_time->time, $finish_time->dim);
  if (isset($incr_time)) printf("incr:   %d - %s\n", $incr_time->time, $incr_time->dim);
}

// ----

the_func(array('start_time' => new RaceTime(20, 's'),
               'finish_time' => new RaceTime(5, 'm'),
               'incr_time' => new RaceTime(3, 'm')));

the_func(array('start_time' => new RaceTime(5, 'm'),
               'finish_time' => new RaceTime(30, 'm')));

the_func(array('start_time' => new RaceTime(30, 'm')));

}
?>
