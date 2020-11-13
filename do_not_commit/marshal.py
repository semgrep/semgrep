import marshal

fin = open('index.mar')
for line in fin:
    # ruleid: marshal-usage
    marshal.dumps(line)

for line in fin:
    # ok
    marshal.someokfunc(line)
