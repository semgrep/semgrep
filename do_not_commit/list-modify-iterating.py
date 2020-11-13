l  = list(range(100))
# ruleid:list-modify-while-iterate
for i in l:
    print(i),
    print(l.pop(0))
    x = l.pop(0)
    print(x)

a = [1, 2, 3, 4]
# ruleid:list-modify-while-iterate
for i in a:
    print(i)
    a.pop(0)

b = [1, 2, 3, 4]
# ruleid:list-modify-while-iterate
for i in b:
    print(i)
    b.append(0)

c = []
# ok
for i in range(5):
    print(i)
    c.append(i)

d = []
e = [1, 2, 3, 4]
# ok
for i in e:
    print(i)
    d.append(i)
