
"""
testing propagating requires 
"""

x.foo(a)
y.foo(b)
z.foo("untainted")

# ruleid: propagator-labels
sink(x)

sink(z)   # foo does not propagate untainted
sink(y)   # foo does not propagate B
sink(any) # sink does not accept default 

"""
testing propagating output labels and requiring labels
"""

c_good.bar(b)
c_bad.bar(any)

# ruleid: propagator-labels
sinkc(c_good) 

sinkc(x)      # sink doesn't accept A
sinkc(c_bad)  # bar does not propagate default
sink(c_good)  # bar does output a different label

"""
testing just propagating output labels 
"""

d1.qux(a)
d2.qux(b)
d3.qux(c_good)
d4.qux(any)

# ruleid: propagator-labels
sinkd(d1)
# ruleid: propagator-labels
sinkd(d2)
# ruleid: propagator-labels
sinkd(d3)
# ruleid: propagator-labels
sinkd(d4)

sinkd(c_good) # sink doesn't accept C
sink(d1)      # qux doesn't output A or B 

"""
testing replacing specific labels
"""

e1.baz(a)
e2.baz(b)
e3.baz(any)

# ruleid: propagator-labels
sinke(e1)
# ruleid: propagator-labels
sinke(e2)
sinke(e3)     # any taint does not satisfy requires  
sinke(c_good) # does not replace label C 