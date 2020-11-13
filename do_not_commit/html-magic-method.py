from django.template import Context, Template
from django.test import SimpleTestCase
from django.utils import html
from django.utils.functional import lazy, lazystr
from django.utils.safestring import SafeData, mark_safe


# cf. https://github.com/django/django/blob/d17b380653da5f95885ce53468fe7aac60672841/tests/utils_tests/test_safestring.py#L8
class customescape(str):
    # ruleid: html-magic-method
    def __html__(self):
        # Implement specific and wrong escaping in order to be able to detect
        # when it runs.
        return self.replace('<', '<<').replace('>', '>>')

class someotherclass(str):
    # ok: html-magic-method
    def __init__(self):
        print('hello')