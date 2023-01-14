<?php
function wrap() {
   $tainted = (sanitizer($source) ? $source : 'ok');
   // ERROR: match 
   sink($tainted);
}
