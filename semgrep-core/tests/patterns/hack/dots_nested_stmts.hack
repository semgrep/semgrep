function foo()
{
    $var = 10;
    
    if ($var === 42) {
        echo('matched');
    }

    //ERROR:
    if ($var === 42) {
        echo('matched');
    } else {
        echo('not matched');
    }
}
