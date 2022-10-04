//ok:test
a = {};
//ok:test
a = {...x};
//ruleid:test
a = {x:1};
//ruleid:test
a = {x:1, y:2}
//ruleid:test
a = {z:0, x:1, y:2, ...u}
