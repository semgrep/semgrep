
using x

using a, b

using x: y

using x as y

# ERROR: match
import x

# ERROR: match
import a, b

# ERROR: match
import x: y

# ERROR: match
import x as y