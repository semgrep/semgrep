from mako.template import Template
from mako import template
import mako
import jinja2

# ruleid:mako-templates-detected
mako.template.Template("hern")
# ruleid:mako-templates-detected
template.Template("hern")
# ruleid:mako-templates-detected
Template("hello")

# ok
t = jinja2.Template("Hello {{ name }}")
t.render(name="world!")