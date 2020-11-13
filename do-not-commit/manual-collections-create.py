foo = {}

# ruleid:manual-defaultdict-dict-create
dict_d = {}
for k, v in foo.items():
    if k not in dict_d:
        dict_d[k] = {}
    dict_d[k].update(v)

# ruleid:manual-defaultdict-set-create
set_d = {}
for k, v in foo.items():
    if k not in set_d:
        set_d[k] = set()
    set_d[k].add(v)

# ruleid:manual-defaultdict-list-create
list_d = {}
for k, v in foo.items():
    if k not in list_d:
        list_d[k] = []
    list_d[k].append(v)

# ruleid:manual-defaultdict-dict-create
setdefault_dict_d = {}
for k, v in foo.items():
    setdefault_dict_d.setdefault(k, {}).update(v)

# ruleid:manual-defaultdict-set-create
setdefault_set_d = {}
for k, v in foo.items():
    setdefault_set_d.setdefault(k, set()).add(v)

# ruleid:manual-defaultdict-list-create
setdefault_list_d = {}
for k, v in foo.items():
    setdefault_list_d.setdefault(k, []).append(v)

# ruleid:manual-counter-create
counter_d = {}
for k, v in foo.items():
    if k not in counter_d:
        counter_d[k] = 0
    counter_d[k] += 1

# okay
for k in foo:
    pass

for k, v in foo.items():
    pass

for k, v in foo.items():
    if k not in [1, 2, 3]:
        pass

result = []
for k, v in foo.items():
    if k not in [1, 2, 3]:
        pass
    result.append(v)
