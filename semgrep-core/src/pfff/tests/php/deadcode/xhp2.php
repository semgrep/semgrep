<?php

/**
 * pad: the quote makes the pfff lexer thinks it's a very long
 * token, which then makes it impossible to merge so different tokens
 * @called-from-phpsh
 */
function pb_merge_func1() {
  echo <x:frag>I don't think so</x:frag>;
//  return <x:frag />;
}

/**
 * @called-from-phpsh
 */
function pb_merge_no_origin_tok() {
  $x = 'pad';
}
