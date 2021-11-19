<?php

$data = $_GET["data"];
// ruleid: tainted-filename
hash_file('sha1', $data);

// ok: tainted-filename
hash_file($data, 'file.txt');
