<?php

interface Iface { }

trait T implements Iface {}

class C { use T; }
function fn(Iface $i) {}

//fn(new C()) // typehint violation
