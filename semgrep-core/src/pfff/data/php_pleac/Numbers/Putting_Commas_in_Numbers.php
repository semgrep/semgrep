# **********************************************************************
# Putting Commas in Numbers
# **********************************************************************
<?php
function pleac_Putting_Commas_in_Numbers() {
// PHP offers the 'number_format' built-in function to, among many other format tasks,
// commify numbers. Perl-compatible [as well as extended] regexes are also available

function commify_series($s) { return number_format($s, 0, '', ','); }

// ------------

$hits = 3456789;

printf("Your website received %s accesses last month\n", commify_series($hits));

// ----------------------------

function commify($s)
{
  return strrev(preg_replace('/(\d\d\d)(?=\d)(?!\d*\.)/', '${1},', strrev($s)));
}

// ------------

$hits = 3456789;

echo commify(sprintf("Your website received %d accesses last month\n", $hits));

}
?>
