# **********************************************************************
# Hashes with Multiple Values Per Key
# **********************************************************************
<?php
function pleac_Hashes_with_Multiple_Values_Per_Key() {
foreach(array_slice(preg_split('/\n/', `who`), 0, -1) as $entry)
{
  list($user, $tty) = preg_split('/\s/', $entry);
  $ttys[$user][] = $tty;

  // Could instead do this:
  // $user = array_slice(preg_split('/\s/', $entry), 0, 2);
  // $ttys[$user[0]][] = $user[1];
}

ksort($ttys);

// ------------

foreach($ttys as $user => $all_ttys)
{
  echo "{$user}: " . join(' ', $all_ttys) . "\n";
}

// ------------

foreach($ttys as $user => $all_ttys)
{
  echo "{$user}: " . join(' ', $all_ttys) . "\n";

  foreach($all_ttys as $tty)
  {
    $stat = stat('/dev/$tty');
    $pwent = posix_getpwuid($stat['uid']);
    $user = isset($pwent['name']) ? $pwent['name'] : 'Not available';
    echo "{$tty} owned by: {$user}\n";
  }
}

}
?>
