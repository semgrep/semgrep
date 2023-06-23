<?php
#ruleid: attributes-match-multiple
#[Attr1]
#[Attr2]
#[Attr3]
function test ()
{
    echo "Test";
}

#ok: attributes-match-multiple
#[Attr1]
function test ()
{
    echo "Test";
}
