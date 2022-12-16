# **********************************************************************
# Adding an Element to a Hash
# **********************************************************************
<?php
function pleac_Adding_an_Element_to_a_Hash() {
$hash[$key] = $value;

// ------------

$food_colour = array('Apple' => 'red', 'Banana' => 'yellow',
                     'Lemon' => 'yellow', 'Carrot' => 'orange');

$food_colour['Raspberry'] = 'pink';

echo "Known foods:\n";
foreach($food_colour as $food => $colour) echo "{$food}\n";

}
?>
