# **********************************************************************
# Creating Persistent Private Variables
# **********************************************************************
<?php
function pleac_Creating_Persistent_Private_Variables() {
// Local scopes are not created in the same way as in Perl [by simply enclosing
// within braces]: only via the creation of functions are local scopes created

// Doesn't create a local scope; '$myvariable' is created at top-level
{
  $myvariable = 7;
}

// '$myvariable' is accessable here
echo $myvariable . "\n";

// ------------

{
  $counter = 0;

  // Local scope created within function, but not within surrounding braces
  // so:
  // * '$counter' is actually a top-level variable, so globally accessable
  // * 'next_counter' has no implict access to '$counter'; must be granted
  //   via 'global' keyword

  function next_counter() { global $counter; $counter++; }
}

// ----------------------------

// PHP doesn't, AFAIK, offer an equivalent to Perl's BEGIN block. Similar
// behaviour may be obtained by defining a class, and including such code
// in its constructor

class BEGIN
{
  private $myvariable;

  function __construct()
  {
    $this->myvariable = 5;
  }

  function othersub()
  {
    echo $this->myvariable . "\n";
  }
}

// ------------

$b = new BEGIN();

$b->othersub();

// ----------------------------

// PHP, like C, supports 'static' local variables, that is, those that upon
// first access are initialised, and thence retain their value between function
// calls. However, the 'counter' example is better implemented as a class

class Counter
{
  private $counter;

  function __construct($counter_init)
  {
    $this->counter = $counter_init;
  }

  function next_counter() { $this->counter++; return $this->counter; }
  function prev_counter() { $this->counter; return $this->counter; }
}

// ------------

$counter = new Counter(42);
echo $counter->next_counter() . "\n";
echo $counter->next_counter() . "\n";
echo $counter->prev_counter() . "\n";

}
?>
