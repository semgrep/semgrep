# **********************************************************************
# Printing Correct Plurals
# **********************************************************************
<?php
function pleac_Printing_Correct_Plurals() {
function pluralise($value, $root, $singular='' , $plural='s')
{
  return $root . (($value > 1) ? $plural : $singular);
}

// ------------

$duration = 1;
printf("It took %d %s\n", $duration, pluralise($duration, 'hour'));
printf("%d %s %s enough.\n", $duration, pluralise($duration, 'hour'),
      pluralise($duration, '', 'is', 'are'));

$duration = 5;
printf("It took %d %s\n", $duration, pluralise($duration, 'hour'));
printf("%d %s %s enough.\n", $duration, pluralise($duration, 'hour'),
      pluralise($duration, '', 'is', 'are'));

// ----------------------------

function plural($singular)
{
  $s2p = array('/ss$/' => 'sses', '/([psc]h)$/' => '${1}es', '/z$/' => 'zes',
               '/ff$/' => 'ffs', '/f$/' => 'ves', '/ey$/' => 'eys',
               '/y$/' => 'ies', '/ix$/' => 'ices', '/([sx])$/' => '$1es',
               '$' => 's');

  foreach($s2p as $s => $p)
  {
    if (preg_match($s, $singular)) return preg_replace($s, $p, $singular);
  }
}

// ------------

foreach(array('mess', 'index', 'leaf', 'puppy') as $word)
{
  printf("%6s -> %s\n", $word, plural($word));
}

}
?>
