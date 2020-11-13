from django.utils.html import conditional_escape
from django.utils.html import escape
from django.utils.html import escapejs
from django.utils.html import format_html
from django.utils.html import html_safe
from django.utils.html import json_script
from django.utils.html import linebreaks
from django.utils.html import smart_urlquote
from django.utils.html import strip_spaces_between_tags
from django.utils.html import strip_tags
from django.utils.html import urlize
from django.utils.safestring import mark_safe

# cf.https://github.com/django/django/blob/76ed1c49f804d409cfc2911a890c78584db3c76e/tests/utils_tests/test_html.py#L204
# ruleid: html-safe
@html_safe
class HtmlClass:
    def __str__(self):
        return "<h1>I'm a html class!</h1>"


# ok: html-safe
class Boring:
    def __str__(self):
        return "<h1>I will become an html class!</h1>"


# ruleid: html-safe
HtmlBoring = html_safe(Boring)
