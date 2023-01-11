<?php
function foo() {
 if ($foo < 1 && $foo > 0) {
 //<xhp-in-comment>
 $a =
 <div>
 <div />
 <div>
 {$foo}
 </div>
 <div>
 {$bar}
 </div>
 </div>;
 $a->appendChild(
 <div>
 <a>
 <fbt desc="foo">Hello there!</fbt>
 </a>
 </div>
 );
 }
}
