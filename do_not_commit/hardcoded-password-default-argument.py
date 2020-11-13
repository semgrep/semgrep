# ok
password = "this-is-probably-a-test"

def say_something(something):
    print(something)

# ok
say_something(password)

# ok
def say_something_else(something_else="something else"):
    print(something_else)

# ruleid:hardcoded-password-default-argument
def whoops(password="this-could-be-bad"):
    print(password)
    
# ok
def ok(password=None):
    print(password)