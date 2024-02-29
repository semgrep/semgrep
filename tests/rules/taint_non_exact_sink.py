#ruleid: test
sink(tainted())

#ruleid: test
sink(ok1 if tainted() else ok2)

#ruleid: test
sink([ok1 if tainted() else ok2])

#ruleid: test
sink(not_a_propagator(tainted()))

#ruleid: test
sink(some_array[tainted()])
