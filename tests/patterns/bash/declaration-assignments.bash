# MATCH:
declare -r a=b

# MATCH:
readonly a=b

# MATCH:
declare a=b c=d

################################################################
# All the matching cases below aren't necessarily desirable.
# It's that the current implementation throws everything away
# except the assignments.
################################################################

# MATCH:
declare a=b

f() {
# MATCH:
  local a=b
}

# MATCH:
a=b

# Maybe this should match even though the definition is local to the command.
a=b command

# MATCH:
declare -r a=b c d
