# https://github.com/returntocorp/semgrep/issues/3127

#OK:
a = foo(data)

#ERROR:
a = foo("foo: bar")

const_str1 = "foo: bar"
#ERROR:
a = foo(const_str1)

const_str2 = 'foo: bar'
#ERROR:
a = foo(const_str2)

const_str3 = "foo: bar\n"
#ERROR:
a = foo(const_str3)

const_str4 = 'foo: bar\n'
#ERROR:
a = foo(const_str4)
