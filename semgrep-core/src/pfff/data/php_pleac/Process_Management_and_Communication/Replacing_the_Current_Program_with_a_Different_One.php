# **********************************************************************
# Replacing the Current Program with a Different One
# **********************************************************************
<?php
function pleac_Replacing_the_Current_Program_with_a_Different_One() {
// Transfer control to the shell to run another program.
pcntl_exec('/bin/sh', array('-c', 'archive *.data'));
// Transfer control directly to another program.
pcntl_exec('/path/to/archive', array('accounting.data'));

}
?>
