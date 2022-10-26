<?php

$id = isset(source()) ? source() : '';

$obj = new Obj($id);

// ok:regression_0113
sink($obj->attr);

$other_obj = new Unsafe();
// ruleid:regression_0113
sink($other_obj->method(source()));
