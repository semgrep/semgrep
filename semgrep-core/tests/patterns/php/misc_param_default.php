<?php    

class TestCasePHPSQL
{
    //public static function testWorking($aaa, $shortName): array
    //{
    //    return $shortName;
    //}

    //ERROR: match
    public static function testNotWorking($aaa, $shortName = 'public'): array
    {
        return $shortName;
    }

}
?>
