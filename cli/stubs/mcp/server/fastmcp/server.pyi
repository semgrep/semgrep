from typing import Any

# Here and elsewhere:
# Defining `getattr` to return `Any` is a workaround which
# allows us to not have to go and define every single attribute
# on the classes we include in the stubs.
#
# We need `Context` here, but I don't care to define everything
# that it possibly has. This works well enough to type-check.
class Context:
    def __getattr__(self, name: str) -> Any: ...

class FastMCP:
    def __getattr__(self, name: str) -> Any: ...

# Apparently, defining this on the rest of the stub also makes it
# so it stops complaining about the rest of the things.
def __getattr__(name: str) -> Any: ...
