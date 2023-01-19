//ok:test
a = {};
//ruleid:test
a = {...x, ...y};
//ruleid:test
a = {...x};
//ruleid:test
a = {...x, y:1}
//ruleid:test
a = {y:1, ...x}
