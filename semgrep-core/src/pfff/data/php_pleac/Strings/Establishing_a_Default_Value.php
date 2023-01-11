# **********************************************************************
# Establishing a Default Value
# **********************************************************************
<?php
function pleac_Establishing_a_Default_Value() {
#-----------------------------
# use $b if $b is true, else $c
$a = $b?$b:$c;

# set $x to $y unless $x is already true
$x || $x=$y;
#-----------------------------
# use $b if $b is defined, else $c
$a = defined($b) ? $b : $c;
#-----------------------------
$foo = $bar || $foo = "DEFAULT VALUE";
#-----------------------------
$dir = array_shift($_SERVER['argv']) || $dir = "/tmp";
#-----------------------------
$dir = $_SERVER['argv'][0] || $dir = "/tmp";
#-----------------------------
$dir = defined($_SERVER['argv'][0]) ? array_shift($_SERVER['argv']) : "/tmp";
#-----------------------------
$dir = count($_SERVER['argv']) ? $_SERVER['argv'][0] : "/tmp";
#-----------------------------
$count[$shell?$shell:"/bin/sh"]++;
#-----------------------------
# find the user name on Unix systems
$user = $_ENV['USER']
     || $user = $_ENV['LOGNAME']
     || $user = posix_getlogin()
     || $user = posix_getpwuid(posix_getuid())[0]
     || $user = "Unknown uid number $<";
#-----------------------------
$starting_point || $starting_point = "Greenwich";
#-----------------------------
count($a) || $a = $b;          # copy only if empty
$a = count($b) ? $b : $c;          # assign @b if nonempty, else @c
#-----------------------------

}
?>
