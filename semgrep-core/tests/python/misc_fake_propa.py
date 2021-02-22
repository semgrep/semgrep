# This used to generate a FakeTokStr exn when we introduced the
# "semantic number" matching feature. Indeed, we were comparing
# first integer values together, but if 2 integers were not equal
# (possibly because there was A None) we were defaulting to
# string comparison by using Parse_info.str_of_info but with
# constant propagation, some values actually don't have a real token
# associated, hence the exn.
# This now works because we fixed Parse_info.str_of_info.

NOSCI_LEN = 14 + 6

def foo():
    hdrlen = NOSCI_LEN

