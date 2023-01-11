# **********************************************************************
# Introduction
# **********************************************************************
<?php
function pleac_Introduction() {
// Since defined at outermost scope, $greeted may be considered a global variable
$greeted = 0;

// ------------

// Must use, 'global', keyword to inform functions that $greeted already exists as
// a global variable. If this is not done, a local variable of that name is implicitly
// defined
function howManyGreetings()
{
  global $greeted;
  return $greeted;
}

function hello()
{
  global $greeted;
  $greeted++;
  echo "high there!, this procedure has been called {$greeted} times\n";
}

// ------------

hello();
$greetings = howManyGreetings();
echo "bye there!, there have been {$greetings} greetings so far\n";

}
?>
