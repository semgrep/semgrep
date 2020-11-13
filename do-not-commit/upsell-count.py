# ruleid:use-count-method
print(Entry.objects.all().len())
# ruleid:use-count-method
print(Entry.objects.get().filter().len())
# ruleid:use-count-method
print(Entry.objects.filter().filter().len())
