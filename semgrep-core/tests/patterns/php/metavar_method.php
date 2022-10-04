<?php

// ERROR:
$wpdb->$get_var("SELECT * FROM wp_users WHERE email = $_POST[email]");

// ERROR:
$wpdb->get_var("SELECT * FROM wp_users WHERE email = $_POST[email]");

?>
