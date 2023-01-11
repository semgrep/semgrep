# **********************************************************************
# Parsing Dates and Times from Strings
# **********************************************************************
<?php
function pleac_Parsing_Dates_and_Times_from_Strings() {
// 'strtotime' parses a textual date expression by attempting a 'best guess' at
// the format, and either fails, or generates a timestamp. Timestamp could be fed
// into any one of the various functions; example:
$timestamp = strtotime('1998-06-03'); echo strftime('%Y-%m-%d', $timestamp) . "\n";

// 'strptime' parses a textual date expression according to a specified format,
// and returns an array of date components; components can be easily dumped
print_r(strptime('1998-06-03', '%Y-%m-%d'));

// ----------------------------

// Parse date string according to format
$darr = strptime('1998-06-03', '%Y-%m-%d');

if (!empty($darr))
{
  // Show date components in 'debug' form
  print_r($darr);

  // Check whether there was a parse error i.e. one or more components could not
  // be extracted from the string
  if (empty($darr['unparsed']))
  {
    // Properly parsed date, so validate required components using, 'checkdate'
    if (checkdate($darr['tm_mon'] + 1, $darr['tm_mday'], $darr['tm_year'] + 1900))
      echo "Parsed date verified as correct\n";
    else
      echo "Parsed date failed verification\n";
  }
  else
  {
    echo "Date string parse not complete; failed components: {$darr['unparsed']}\n";
  }
}
else
{
  echo "Date string could not be parsed\n";
}

}
?>
