<?php

// ERROR: match
interface Single extends Serializable {
    function SomeFunc();
}

// ERROR: match
interface Front extends Serializable, Throwable, Stringable {
    function SomeFunc();
}

// ERROR: match
interface Middle extends Throwable, Serializable, Stringable {
    function SomeFunc();
}

// ERROR: match
interface Last extends Throwable, Stringable, Serializable {
    function SomeFunc();
}
