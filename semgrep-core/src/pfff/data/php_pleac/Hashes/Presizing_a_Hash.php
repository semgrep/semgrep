# **********************************************************************
# Presizing a Hash
# **********************************************************************
<?php
function pleac_Presizing_a_Hash() {
// PHP hashes are dynamic expanding and contracting as entries are added, and removed,
// respectively. Thus, there is no need to presize a hash, nor is there, AFAIK, any
// means of doing so except by the number of datums used when defining the hash

// zero elements
$hash = array();

// ------------

// three elements
$hash = array('Apple' => 'red', 'Lemon' => 'yellow', 'Carrot' => 'orange');

}
?>
