# ruleid:raise-not-base-exception
raise "error here"

# todoruleid:raise-not-base-exception
raise 5


class Foobar:
    x = 5


# todoruleid:raise-not-base-exception
raise Foobar()


class Foobar2(BaseException):
    x = 5


# OK
raise Foobar2()

# OK
raise Exception()
