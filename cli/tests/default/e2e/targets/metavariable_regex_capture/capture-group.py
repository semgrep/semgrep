import hashlib

# A named capture group (?<ALG>...) binds $ALG, which is interpolated into the
# rule message. Regression target for ENGINE-2932: we must get exactly one
# finding here, not a duplicate whose message still reads "a hash $ALG ...".
hashlib.new("Sha512_256")
