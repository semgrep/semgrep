import hashlib

# The capture-group regex matches the metavariable value three times, so we
# expect three findings, each with $ALG substituted to a distinct value
# (ENGINE-2932: every binding-set is preserved, none left unsubstituted).
hashlib.new("Sha1Sha256Sha384")
