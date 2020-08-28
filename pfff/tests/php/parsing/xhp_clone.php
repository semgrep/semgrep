<?php

class X {
   private function getContentLength() {
     // clone incase any children %phrase happen to be :m:base subclasses
    $children = clone <x:frag>{$this->getChildren()}</x:frag>;
   }
}

