


#int test
def reverse_test(xs):
    for i in xs:
        print i
    if len(xs) == 0:
        print "my_reverse: null"
    else:
        print "my_reverse: not null"
    xs.reverse()
    return xs

#string test
def append_test(xs):
    sentence = "this is a melon"
    words = sentence.split()
    return (words + xs)
