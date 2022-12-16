# **********************************************************************
# Introduction
# **********************************************************************
<?php
function pleac_Introduction() {
// PHP's date / time suport is quite extensive, and appears grouped into three areas of
// functionality:
//
// * UNIX / C Library [libc]-based routines, which include [among others]:
//   - localtime, gmtime
//   - strftime, strptime, mktime
//   - time, getdate, gettimeofday,
//
// * PHP 'native' functions, those date / time routines released in earlier versions,
//   and which otherwise provide 'convenience' functionality; these include:
//   - date
//   - strtotime
//
// * 'DateTime' class-based. This facility appears [according to the PHP documentation]
//   to be extremely new / experimental, so whilst usage examples will be provided, they
//   should not be taken to be 'official' examples, and obviously, subject to change.
//   My own impression is that this facility is currently only partially implemented,
//   so there is limited use for these functions. The functions included in this group
//   are some of the 'date_'-prefixed functions; they are, however, not used standalone,
//   but as methods in conjunction with an object. Typical usage:
//
//     $today = new DateTime();             // actually calls: date_create($today, ...);
//     echo $today->format('U') . "\n";     // actually calls: date_format($today, ...);
//
// Also worth mentioning is the PEAR [PHP Extension and Repository] package, 'Calendar',
// which offers a rich set of date / time manipulation facilities. However, since it is
// not currently shipped with PHP, no examples appear

// Helper functions for performing date arithmetic

function dateOffset()
{
  static $tbl = array('sec' => 1, 'min' => 60, 'hou' => 3600, 'day' => 86400, 'wee' => 604800);
  $delta = 0;

  foreach (func_get_args() as $arg)
  {
    $kv = explode('=', $arg);
    $delta += $kv[1] * $tbl[strtolower(substr($kv[0], 0, 3))];
  }

  return $delta;
}

function dateInterval($intvltype, $timevalue)
{
  static $tbl = array('sec' => 1, 'min' => 60, 'hou' => 3600, 'day' => 86400, 'wee' => 604800);
  return (int) round($timevalue / $tbl[strtolower(substr($intvltype, 0, 3))]);
}

// ----------------------------

// Extract indexed array from 'getdate'
$today = getdate();
printf("Today is day %d of the current year\n", $today['yday']);

// Extract indexed, and associative arrays, respectively, from 'localtime'
$today = localtime();
printf("Today is day %d of the current year\n", $today[7]);

$today = localtime(time(), TRUE);
printf("Today is day %d of the current year\n", $today['tm_yday']);

}
?>
