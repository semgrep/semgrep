#ruleid: test
sink(tainted())

#ok:test
sink(ok1 if tainted() else ok2)

#ok:test
sink([ok1 if tainted() else ok2])

#ok:test
sink(not_a_propagator(tainted()))

#ok:test
sink(some_array[tainted()])
