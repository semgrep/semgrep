def foo(request):
    # ERROR: open-from-request
    path = request.get("unsafe")
    open(path)

def bar(request):
    foo = request.get("unsafe")
    path = "safe_path"
    open(path)

# This _should_ be detected, but is not
# due to semgrep#707
z = request.get("unsafe")
def baz():
    open(z)

