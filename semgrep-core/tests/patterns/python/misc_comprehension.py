
# this used to not include the trailing ']' in the matched range

def ends_with_comprehension():
    #ERROR: match
    thing = [x for x in range(10)]
