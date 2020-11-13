import copy

# ruleid: default-mutable-list
def append_func1(default=[]):
    default.append(5)


# ruleid: default-mutable-list
def append_func2(default=[]):
    for x in range(10):
        default.append(x)


# todoruleid: default-mutable-list
def append_func3(default=[]):
    x = default
    x.append(5)


# ruleid: default-mutable-list
def append_func4(x=1, default=[]):
    default.append(5)


# ruleid: default-mutable-list
def append_func5(default=[]):
    if not default:
        default.append(1)


# ruleid: default-mutable-list
def append_func6(default=[], x="string"):
    default.append(5)


# todoruleid: default-mutable-list
def append_func7(default=[]):
    if True:
        default = list(default)
    else:
        default.append(1)


# ruleid: default-mutable-list
def append_func8(default=[]):
    while True:
        default.append(1)
        break


# ruleid: default-mutable-list
def extend_func1(default=[]):
    default.extend([5])


# ruleid: default-mutable-list
def extend_func2(default=[]):
    for x in range(10):
        default.extend([x])


# todoruleid: default-mutable-list
def extend_func3(default=[]):
    x = default
    x.extend([5])


# ruleid: default-mutable-list
def extend_func4(x=1, default=[]):
    default.extend([5])


# ruleid: default-mutable-list
def extend_func5(default=[]):
    if not default:
        default.extend([1])


# ruleid: default-mutable-list
def extend_func6(default=[], x="string"):
    default.extend([5])


# todoruleid: default-mutable-list
def extend_func7(default=[]):
    if True:
        default = list(default)
    else:
        default.extend([1])


# ruleid: default-mutable-list
def extend_func8(default=[]):
    while True:
        default.extend([1])
        break


# ruleid: default-mutable-list
def insert_func1(default=[]):
    default.insert(0, 5)


# ruleid: default-mutable-list
def insert_func2(default=[]):
    for x in range(10):
        default.insert(0, x)


# todoruleid: default-mutable-list
def insert_func3(default=[]):
    x = default
    x.insert(0, 5)


# ruleid: default-mutable-list
def insert_func4(x=1, default=[]):
    default.insert(0, 5)


# ruleid: default-mutable-list
def insert_func5(default=[]):
    if not default:
        default.insert(0, 1)


# ruleid: default-mutable-list
def insert_func6(default=[], x="string"):
    default.insert(0, 5)


# todoruleid: default-mutable-list
def insert_func7(default=[]):
    if True:
        default = list(default)
    else:
        default.insert(0, 1)


# ruleid: default-mutable-list
def insert_func8(default=[]):
    while True:
        default.insert(0, 1)
        break


##### Should not fire on anything below this

# OK
def not_append_func0(x=1):
    x = []
    x.append(2)


# OK
def not_append_func1(default=[]):
    # Immediately overwrites default list
    default = []
    default.append(5)


# OK
def not_append_func2(default=[]):
    # list() returns a copy
    default = list(default)
    default.append(5)


# OK
def not_append_func3(default=[]):
    # copy.deepcopy returns a copy
    default = copy.deepcopy(default)
    default.append(5)


# OK
def not_append_func3_1(default=[]):
    # copy.deepcopy returns a copy
    default = copy.copy(default)
    default.append(5)


# OK
def not_append_func4(default=[]):
    # list.copy returns a copy
    default = list.copy(default)
    default.append(5)


# OK
def not_append_func5(default=[]):
    # [:] returns a copy
    default = default[:]
    default.append(5)


# OK
def append_wrapper():
    x = 1
    # OK
    def not_append_func6(default=[]):
        default.append(5)

    not_append_func6()


# OK
def not_append_func7(default=[]):
    if default is []:
        return 5 + 1


# OK
def not_append_func8(default=[]):
    default = default or []
    default.append(5)


# OK
def not_append_func9(default=[]):
    default = list()
    default.append(5)


# OK
def not_append_func10(default=[]):
    default = [str(x) for x in default]
    default.append(5)


# OK
def not_insert_func0(x=1):
    x = []
    x.insert(0, 2)


# OK
def not_insert_func1(default=[]):
    # Immediately overwrites default list
    default = []
    default.insert(0, 5)


# OK
def not_insert_func2(default=[]):
    # list() returns a copy
    default = list(default)
    default.insert(0, 5)


# OK
def not_insert_func3(default=[]):
    # copy.deepcopy returns a copy
    default = copy.deepcopy(default)
    default.insert(0, 5)


# OK
def not_insert_func3_1(default=[]):
    # copy.deepcopy returns a copy
    default = copy.copy(default)
    default.insert(0, 5)


# OK
def not_insert_func4(default=[]):
    # list.copy returns a copy
    default = list.copy(default)
    default.insert(0, 5)


# OK
def not_insert_func5(default=[]):
    # [:] returns a copy
    default = default[:]
    default.insert(0, 5)


# OK
def insert_wrapper():
    x = 1
    # OK
    def not_insert_func6(default=[]):
        default.insert(0, 5)

    not_insert_func6()


# OK
def not_insert_func7(default=[]):
    if default is []:
        return 5 + 1


# OK
def not_insert_func8(default=[]):
    default = default or []
    default.insert(0, 5)


# OK
def not_insert_func9(default=[]):
    default = list()
    default.insert(0, 5)


# OK
def not_insert_func10(default=[]):
    default = [str(x) for x in default]
    default.insert(0, 5)


# OK
def not_extend_func0(x=1):
    x = []
    x.extend([2])


# OK
def not_extend_func1(default=[]):
    # Immediately overwrites default list
    default = []
    default.extend([5])


# OK
def not_extend_func2(default=[]):
    # list() returns a copy
    default = list(default)
    default.extend([5])


# OK
def not_extend_func3(default=[]):
    # copy.deepcopy returns a copy
    default = copy.deepcopy(default)
    default.extend([5])


# OK
def not_extend_func3_1(default=[]):
    # copy.deepcopy returns a copy
    default = copy.copy(default)
    default.extend([5])


# OK
def not_extend_func4(default=[]):
    # list.copy returns a copy
    default = list.copy(default)
    default.extend([5])


# OK
def not_extend_func5(default=[]):
    # [:] returns a copy
    default = default[:]
    default.extend([5])


# OK
def extend_wrapper():
    x = 1
    # OK
    def not_extend_func6(default=[]):
        default.extend([5])

    not_extend_func6()


# OK
def not_extend_func7(default=[]):
    if default is []:
        return 5 + 1


# OK
def not_extend_func8(default=[]):
    default = default or []
    default.extend([5])


# OK
def not_extend_func9(default=[]):
    default = list()
    default.extend([5])


# OK
def not_extend_func10(default=[]):
    default = [str(x) for x in default]
    default.extend([5])
