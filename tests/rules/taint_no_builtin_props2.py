def test():
    file = open("test.txt")
    file.close()
    # While working on PR #9273 we introduced a regression that caused the above
    # sanitizer to not work.
    #ok: test
    sink(file)
