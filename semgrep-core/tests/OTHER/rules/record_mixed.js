//ok:test
a = {};
//ok:test
a = {...x};
//ruleid:test
a = {x:1, ...y};
//ok:test
a = {x:1, y:2}
//ruleid:test
a = {u:0, ...z, x:1, y:2}
