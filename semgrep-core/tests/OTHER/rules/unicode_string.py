# Both 🎅 and 🐻 are UTF-8-encoded using 3 bytes.

#ruleid:
"🎅"

# The following shouldn't match the "🎅" pattern but it does, due to
# our Unicode hack that sees all non-ascii bytes as Zs. Remove the 'ruleid'
# line once the Unicode hack is gone.
#ruleid:
"🐻"
