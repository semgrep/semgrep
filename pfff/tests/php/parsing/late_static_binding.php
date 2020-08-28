<?php

//src: http://php.net/manual/en/language.oop5.late-static-bindings.php

class A {
    public static function who() {
        echo __CLASS__;
    }
    public static function test() {
        static::who(); // Here comes Late Static Bindings
    }
}

class B extends A {
    public static function who() {
        echo __CLASS__;
    }
}

// should print "B"
B::test();
