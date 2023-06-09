# https://github.com/returntocorp/semgrep/issues/7893

# This requires Pro
# from constants import ONE_DAY
ONE_DAY = 60 * 60 * 60 * 24

MANY_DAYS = 5 * ONE_DAY
ONE = 1
TWO = 2 * ONE
FIVE = 5 * ONE

def bad(var):
    print(var)

# ok: test
bad(var=2)
# ok: test
bad(var=TWO)
# ruleid: test
bad(var=3)
# ruleid: test
bad(var=FIVE)
# ruleid: test
bad(var=ONE_DAY)
# ruleid: test
bad(var=3000)
# ruleid: test
bad(var=MANY_DAYS)
