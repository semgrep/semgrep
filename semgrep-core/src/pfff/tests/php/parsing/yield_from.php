<?php

class Foo
{
    public function provideCheckSettings() {
        yield from parent::provideCheckSettings();
    }

}
