# **********************************************************************
# Operating on a Series of Integers
# **********************************************************************
<?php
function pleac_Operating_on_a_Series_of_Integers() {
foreach (range($X, $Y) as $i)
{
  ; // ...
}

foreach (range($X, $Y, 7) as $i)
{
  ; // ...
}

for ($i = $X; $i <= $Y; $i++)
{
  ; // ...
}

for ($i = $X; $i <= $Y; $i += 7)
{
  ; // ...
}

// ----------------------------

echo 'Infancy is:'; foreach(range(0, 2) as $i) echo " {$i}\n";
echo 'Toddling is:'; foreach(range(3, 4) as $i) echo " {$i}\n";
echo 'Childhood is:'; foreach(range(5, 12) as $i) echo " {$i}\n";

}
?>
