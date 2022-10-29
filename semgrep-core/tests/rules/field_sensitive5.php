<?php

$id = isset(source()) ? source() : '';

$obj = new Obj($id);

// We have a sanitizer cleaning $obj right here
// ok:regression_0.113.0
sink($obj->attr);

$other_obj = new Unsafe();
// ruleid:regression_0.113.0
sink($other_obj->method(source()));
