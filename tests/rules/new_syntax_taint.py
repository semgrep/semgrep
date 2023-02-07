
x = tainted1
# ruleid: new-syntax-taint
sink(x)

x = tainted1
# ruleid: new-syntax-taint
sink2(x)

x = tainted2([])
# ruleid: new-syntax-taint
sink(x)


y = tainted1
z = clean(y)
sink(z)

y = tainted1
z = clean2(y)
sink(z)

y = tainted1 
dict = 0
dict.foo(y)
# ruleid: new-syntax-taint
sink(dict)


