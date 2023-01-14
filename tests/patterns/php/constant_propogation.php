<?php

class Example
{
    const CMD = 'This is an constant.';

    public function someMethod()
    {
        // ERROR:
        exec(self::CMD);
    }
}
