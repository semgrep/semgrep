def foo(request):
    # ERROR: open-from-request
    path = request.get("unsafe")
    open(path)

def bar(request):
    foo = request.get("unsafe")
    path = "safe_path"
    open(path)

# ERROR: open-from-request
z = request.get("unsafe")
def baz():
    open(z)

