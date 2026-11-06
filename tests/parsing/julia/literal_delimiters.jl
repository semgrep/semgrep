s = "hello $name $(f(x=1))\n"
t = """hello "quoted" $name $(f(x))
next"""
c = `echo $name $(f(x))`
d = ```echo `quoted` $name $(f(x))```
r = raw"$name \n \"quoted\""
rt = raw"""$name "quoted" \n"""
p = r"a\d+"im
pc = cmd`$name \n`
pct = cmd```$name `quoted` \n```
