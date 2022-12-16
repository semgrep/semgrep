# **********************************************************************
# Making Numbers Even More Random
# **********************************************************************
<?php
function pleac_Making_Numbers_Even_More_Random() {
// The above is considered - for many reasons - a poor way of seeding the RNG. PHP
// also offers alternate versions of the functions, 'mt_srand' and 'mt_rand',
// which are described as faster, and more 'random', though key to obtaining a
// more 'random' distribution of generated numbers seems to be through using
// a combination of a previously saved random value in combination with an
// unrepeatable value [like the current time in microseconds] that is multiplied
// by a large prime number, or perhaps as part of a hash [examples available in
// PHP documentation for 'srand' and 'mt_srand']

mt_srand($saved_random_value + microtime() * 1000003);

// or

mt_srand(($saved_random_value + hexdec(substr(md5(microtime()), -8))) & 0x7fffffff);

// Use of 'mt_rand' together with an appropriate seeding approach should help better
// approximate the generation of a 'truly random value'
$truly_random_value = mt_rand();

}
?>
