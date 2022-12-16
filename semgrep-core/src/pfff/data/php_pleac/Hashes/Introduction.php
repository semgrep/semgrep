# **********************************************************************
# Introduction
# **********************************************************************
<?php
function pleac_Introduction() {
// PHP uses the term 'array' to refer to associative arrays - referred to in Perl
// as 'hashes' - and for the sake of avoiding confusion, the Perl terminology will
// be used. As a matter of interest, PHP does not sport a vector, matrix, or list
// type: the 'array' [Perl 'hash'] serves all these roles

$age = array('Nat' => 24, 'Jules' => 25, 'Josh' => 17);

$age['Nat'] = 24;
$age['Jules'] = 25;
$age['Josh'] = 17;

$age = array_combine(array('Nat', 'Jules', 'Josh'), array(24, 25, 17));

// ------------

$food_colour = array('Apple' => 'red', 'Banana' => 'yellow',
                     'Lemon' => 'yellow', 'Carrot' => 'orange');

$food_colour['Apple'] = 'red'; $food_colour['Banana'] = 'yellow';
$food_colour['Lemon'] = 'yellow'; $food_colour['Carrot'] = 'orange';

$food_colour = array_combine(array('Apple', 'Banana', 'Lemon', 'Carrot'),
                             array('red', 'yellow', 'yellow', 'orange'));

}
?>
