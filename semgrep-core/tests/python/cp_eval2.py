# https://github.com/returntocorp/semgrep/issues/2616
s = "print('This is a safe eval')"
s += argv[1]
# OK:
eval(s)

