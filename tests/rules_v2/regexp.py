def test():
    #ruleid: rule_template_id
    foo("128.0.0.1")

    foo("this is not an IP")

    foo("neither this")

    # this is an IP but in comment foo("128.0.0.1")
