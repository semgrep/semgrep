<?php

async function global_foo() {
  $f = async function ($a, $b) { return 123; };
  return 123;
}

class A {
  public async function memberFoo1() {
    return 123;
  }

  async public function memberFoo2() {
    return 123;
  }

  static async private function memberFoo3() {
    return 123;
  }
}
