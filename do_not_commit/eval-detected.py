# ok
eval("x = 1; x = x + 2")

blah = "import requests; r = requests.get('https://example.com')"
# ok
eval(blah)

dynamic = "import requests; r = requests.get('{}')"
# ruleid:eval-detected
eval(dynamic.format("https://example.com"))

def eval_something(something):
    # ruleid:eval-detected
    eval(something)

from something import eval

# ok
eval("something")
