
#ERROR: match, list in argument
def not_append_func10(default=[]):
    #ERROR: match, comprehension list is a list
    default = [str(x) for x in default]
    default.append(5)
