x = object()

# ruleid:identical-is-comparison
if x is x:
  print('true')

if x is None: # ok
  pass

if (type(X) is str):
  pass # OK

if x is True:
  pass # OK

if x is False:
  pass # OK

# ruleid: string-is-comparison
if x is 'hello there':
  pass
  
# ruleid: string-is-comparison
if "hello there" is x:
  pass

# OK; technically implementation-defined I think, but maybe not worth warning about
if x is '':
  pass
