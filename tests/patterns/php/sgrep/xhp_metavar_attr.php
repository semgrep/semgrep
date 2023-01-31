<?php

// $ sgrep -e '<X Y={$V->toString()}></X>' *
function test_xhp_metavar_attr() {
  $x = null;
  $xhp = <x:frag lbl={$x->foo()->toString()}></x:frag>;
}
