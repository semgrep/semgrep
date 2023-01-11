# **********************************************************************
# Hashing References
# **********************************************************************
<?php
function pleac_Hashing_References() {
// PHP implements a special type known as a 'resource' that encompasses things like file handles,
// sockets, database connections, and many others. The 'resource' type is, essentially, a
// reference variable that is not readily serialisable. That is to say:
// * A 'resource' may be converted to a string representation via the 'var_export' function
// * That same string cannot be converted back into a 'resource'
// So, in terms of array handling, 'resource' types may be stored as array reference values,
// but cannot be used as keys.
//
// I suspect it is this type of problem that the Perl::Tie package helps resolve. However, since
// PHP doesn't, AFAIK, sport a similar facility, the examples in this section cannot be
// implemented using file handles as keys

$filenames = array('/etc/termcap', '/vmlinux', '/bin/cat');

foreach($filenames as $filename)
{
  if (!($fh = fopen($filename, 'r'))) continue;

  // Cannot do this as required by the Perl code:
  // $name[$fh] = $filename;

  // Ok
  $name[$filename] = $fh;
}

// Would traverse array via:
//
// foreach(array_keys($name) as $fh)
// ...
// or
//
// foreach($name as $fh => $filename)
// ...
// but since '$fh' cannot be a key, either of these will work:
//
// foreach($name as $filename => $fh)
// or
foreach(array_values($name) as $fh)
{
  fclose($fh);
}

}
?>
