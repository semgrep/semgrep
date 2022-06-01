<?php
class SomeClass extends \SomePkg\SomeBase
{
    //ERROR: match
    public function someMethod(\SomePkg\Other $var)
    {
        return null;
    }
}
?>
