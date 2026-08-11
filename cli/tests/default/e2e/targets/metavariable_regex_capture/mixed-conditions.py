import hashlib

# A binding metavariable-regex ($ALG) combined with a non-binding one ($MODE).
# Regression target for ENGINE-2932: the empty binding-set from the non-binding
# condition must not produce a second, unsubstituted "a hash $ALG ..." finding.
hashlib.new("Sha512_256", usedforsecurity)
