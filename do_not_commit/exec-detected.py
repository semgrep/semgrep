# ok
exec("x = 1; x = x + 2")

blah = "import requests; r = requests.get('https://example.com')"
# ok
exec(blah)

dynamic = "import requests; r = requests.get('{}')"
# ruleid:exec-detected
exec(dynamic.format("https://example.com"))

def eval_something(something):
    # ruleid:exec-detected
    exec(something)

from something import exec

# ok
exec("something")