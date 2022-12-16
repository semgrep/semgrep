<?php

$sql_remove_with_a_name_that_is_way_tooooo_long =
  'DELETE FROM X WHERE id = %d AND netwo rk_key = %d a b c d e f ' . ' ggggggg';



$sql_remove = 'DELETE FROM X WHERE id = %d AND network_key = %d a' .
              ' b c d e f ggggggg';

function deactivate_affiliations($userid) {
  $sql = 'SELECT `network_key` FROM `data` ' .
         'WHERE `id` = %d AND `status` != %d AND `confirmed` = %d';

}
