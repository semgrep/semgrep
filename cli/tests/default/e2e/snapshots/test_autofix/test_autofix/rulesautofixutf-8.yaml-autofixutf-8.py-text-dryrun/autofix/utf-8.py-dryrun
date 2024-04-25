# Tests autofixes when non-ascii characters are present in the source file and
# the fix.
#
# In Python, strings are sequences of characters. In OCaml, they are sequences
# of bytes. Because of this, we need to be particularly careful that we do not
# conflate bytes with characters.

# U+1F60A. Has a 4 byte encoding in utf-8. This is here to throw off offsets
# later in the file if we are computing them incorrectly.
x = '😊'

print('a')

# And this one is here at the beginning of a line to throw off columns later in
# the line if we are computing them incorrectly.
y = '😊'; print('a'); print('a')
