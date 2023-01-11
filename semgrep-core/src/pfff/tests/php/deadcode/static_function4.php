<?php

class SF4_A {
  public static function not_dead() {
  }
}

class SF4_B extends SF4_A {

}

function main() {

  // This was causing some pbs in the deadcode analyzer and in the
  // callgraph extractor.
  SF4_A::not_dead();
  SF4_B::not_dead();
}
