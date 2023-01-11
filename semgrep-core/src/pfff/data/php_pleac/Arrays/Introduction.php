# **********************************************************************
# Introduction
# **********************************************************************
<?php
function pleac_Introduction() {
// Nested arrays are supported, and may be easily printed using 'print_r'

$nested = array('this', 'that', 'the', 'other');

$nested = array('this', 'that', array('the', 'other')); print_r($nested);

$tune = array('The', 'Star-Spangled', 'Banner');

}
?>
