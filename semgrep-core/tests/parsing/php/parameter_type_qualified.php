<?php
// https://semgrep.dev/s/idawson-gl:findmethod
// from https://github.com/returntocorp/semgrep/issues/4589
class SomeClass extends \SomePkg\SomeBase
{
    public function someMethod(\SomePkg\Other $var)
    {
        return null;
    }
}
?>
