# ERROR:
/a/

# ERROR:
/[0-9]/

# ERROR:
/.+/

# ERROR:
/\./

# ERROR:
/\w+/

# ERROR:
/\\/

# ERROR:
/\z/

# ERROR:
/\a\z/

# TODO to be consistent with interpolation in strings, we should match here
/foo#{2 + 2}/
