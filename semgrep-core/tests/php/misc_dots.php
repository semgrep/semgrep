<?php

$ch = curl_init();

//ERROR: match
$verify = false;
curl_setopt($ch, CURLOPT_URL, "http://www.example.com/");
curl_setopt($ch, CURLOPT_HEADER, 0);

// This is caught 
curl_setopt($ch, CURLOPT_SSL_VERIFYPEER, false);

// ... but can I catch this?
curl_setopt($ch, CURLOPT_SSL_VERIFYPEER, $verify);

curl_exec($ch);
curl_close($ch);
