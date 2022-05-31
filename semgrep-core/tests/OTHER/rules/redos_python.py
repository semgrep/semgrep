import re

# ruleid: redos-python
re.match('(a+)+$', ...)

re.match('hello+', ...)

def test_patterns(input_str):
    # ruleid: redos-python
    vulnerable = re.match('((honk )+)+$', input_str)
    ok         = re.match('((honk )+)++$', input_str)

def validate_email(email_str):
    # ruleid: redos-python
    re.match(
        '^([a-zA-Z0-9])(([\-.]|[_]+)?([a-zA-Z0-9]+))*(@){1}[a-z0-9]+[.]{1}(([a-z]{2,3})|([a-z]{2,3}[.]{1}[a-z]{2,3}))$',
        email_str
    )
