# **********************************************************************
# Specifying a List In Your Program
# **********************************************************************
<?php
function pleac_Specifying_a_List_In_Your_Program() {
// PHP offers only the 'array' type which is actually an associative array, though
// may be numerically indexed, to mimic vectors and matrices; there is no separate
// 'list' type

$a = array('quick', 'brown', 'fox');

// ------------

$a = escapeshellarg('Why are you teasing me?');

// ------------

$lines = <<<END_OF_HERE_DOC
    The boy stood on the burning deck,
    it was as hot as glass.
END_OF_HERE_DOC;

// ------------

$bigarray = array_map('rtrim', file('mydatafile'));

// ------------

$banner = 'The mines of Moria';

$banner = escapeshellarg('The mines of Moria');

// ------------

$name = 'Gandalf';

$banner = "Speak {$name}, and enter!";

$banner = 'Speak ' . escapeshellarg($name) . ' and welcome!';

// ------------

$his_host = 'www.perl.com';

$host_info = `nslookup $his_host`;

$cmd = 'ps ' . posix_getpid(); $perl_info = `$cmd`;

//$shell_info = `ps $$`;

// ------------

$banner = array('Costs', 'only', '$4.95');

$banner = array_map('escapeshellarg', split(' ', 'Costs only $4.95'));

// ------------

// AFAIK PHP doesn't support non-quoted strings ala Perl's 'q', 'qq' and 'qw', so arrays
// created from strings must use quoted strings, and make use of 'split' [or equivalent].
// A slew of functions exist for performing string quoting, including 'escapeshellarg',
// 'quotemeta', and 'preg_quote'

$brax = split(' ', '( ) < > { } [ ]');

// Do this to quote each element within '..'
// $brax = array_map('escapeshellarg', split(' ', '( ) < > { } [ ]'));

$rings = split(' ', 'Nenya Narya Vilya');

$tags = split(' ', 'LI TABLE TR TD A IMG H1 P');

$sample = split(' ', 'The vertical bar | looks and behaves like a pipe.');

}
?>
