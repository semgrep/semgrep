# **********************************************************************
# Program: fixstyle
# **********************************************************************
<?php
function pleac_Program__fixstyle() {
#-----------------------------
# @@INCLUDE@@ include/php/fixstyle.php
#-----------------------------
# @@INCLUDE@@ include/php/fixstyle2.php
#-----------------------------
// very fast, but whitespace collapse
while (!feof($input)) {
  $i = 0;
  preg_match("/^(\s*)(.*)/", fgets($input), $matches); // emit leading whitespace
  fwrite($output, $matches[1]);
  foreach (preg_split("/(\s+)/", $matches[2]) as $token) { // preserve trailing whitespace
    fwrite($output, (array_key_exists($token, $config) ? $config[$token] : $token) . " ");
  }
  fwrite($output, "\n");
}
#-----------------------------

}
?>
