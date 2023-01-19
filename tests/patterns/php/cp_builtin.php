<?php
// https://linear.app/r2c/issue/PA-870

function test() {
    $dir="/home"."/test2";
    $cmd="rm -rf ".escapeshellarg($dir);
    //ERROR:
    exec($cmd);
}
