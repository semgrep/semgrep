# **********************************************************************
# Returning Failure
# **********************************************************************
<?php
function pleac_Returning_Failure() {
// AFAICT, most of the PHP library functions are designed to return some required
// value on success, and FALSE on exit. Whilst it is possible to return NULL, or
// one of the recognised 'empty' values [e.g. '' or 0 or an empty array etc],
// FALSE actually seems to be the preferred means of indicating failure

function a_func() { return FALSE; }

a_func() || die("Function failed\n");

if (!a_func()) die("Function failed\n");

}
?>
