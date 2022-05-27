<?php

class Number {
  public function __construct(
    private int|float $number
  ) {}
}

new Number('NaN'); // TypeError
