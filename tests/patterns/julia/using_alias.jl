
using x

using a, b

using x: y

# ERROR: match
using x as y

import x

import a, b

import x: y

import x as y