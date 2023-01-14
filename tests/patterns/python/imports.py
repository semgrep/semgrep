# generated manually from https://docs.python.org/3/reference/grammar.html
# regular imports

#ERROR:
# as imports

#ERROR:
import bar as b
#ERROR:
import foo.bar as b
#ERROR:

# from imports
#ERROR:
from foo import bar
from .foo import bar
from ...foo import bar
from ..foo import bar
#ERROR:
from foo.bar import baz
from .foo.bar import baz
from ...foo.bar import baz
from ..foo.bar import baz
from . import bar
from . import bar
from .. import bar
from ... import bar

#ERROR:
from foo import bar

#ERROR:
from foo import bar

#ERROR:

# * imports
#ERROR:

# , imports

#ERROR:
from foo import bar, baz
from .foo import bar, baz
from ...foo import bar, baz
from ..foo import bar, baz
#ERROR:
from foo.bar import baz, qux
from .foo.bar import baz, qux
from ...foo.bar import baz, qux
from ..foo.bar import baz, qux
from . import bar, baz

# ( ) imports
#ERROR:
from foo import (bar, baz)
from .foo import (bar, baz)
from ...foo import (bar, baz)
from ..foo import (bar, baz)
#ERROR:
from foo.bar import (baz, qux)
from .foo.bar import baz
from ...foo.bar import baz
from ..foo.bar import baz
from . import bar,

# as imports
#ERROR:
from foo import baz as b
from .foo import baz as b
#ERROR:

# tricky
