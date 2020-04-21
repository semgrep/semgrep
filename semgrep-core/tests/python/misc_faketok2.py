def foo():

    #ERROR: match
    logging.info(f"Current bento version is {current_version}, latest is {latest_version}")
    # note that this code is actually translated internally in
    # a call to IdSpecial(Concat), with the different subcomponents,
    # so a pattern like $X("...") will not match this (interpolated) string

