# ruleid:use-earliest-or-latest
Entry.objects.order_by()[0]
# OK
Entry.objects.order_by()[1]

# ruleid:use-earliest-or-latest
Entry.objects.all().order_by('foo')[0]
# OK
Entry.objects.all().order_by('foo')[1]

# ruleid:use-earliest-or-latest
Entry.objects.all().filter().order_by('foo')[0]
# OK
Entry.objects.all().filter().order_by('foo')[1]

def order_by(foo: str): pass
# OK
order_by("testing")