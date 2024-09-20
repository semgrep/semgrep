<?php
function wrap() {
   $tainted = (sanitizer($source) ? $source : 'ok');
   // ruleid: taint-not-detected
   sink($tainted);
}
