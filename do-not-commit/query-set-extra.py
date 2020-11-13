# ruleid:avoid-query-set-extra
Entry.objects.get().extra()

# ruleid:avoid-query-set-extra
Entry.objects.filter().extra()

# ruleid:avoid-query-set-extra
Entry.objects.update().extra()

# ruleid:avoid-query-set-extra
Entry.objects.filter().udpate().extra()

# ruleid:avoid-query-set-extra
Entry.objects.get({}).filter().udpate().extra()
