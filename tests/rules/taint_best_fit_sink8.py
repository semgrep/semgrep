#ruleid: test
sink([tainted(), safe()])

#ok: test
sink([safe(), tainted()])

#ok: test
sink([safe(), safe()])

#ok: test
sink([ok1 if tainted() else ok2, safe()])
