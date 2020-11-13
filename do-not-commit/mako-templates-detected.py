import jinja2
import mako
from mako import template
from mako.template import Template

# ruleid:mako-templates-detected
mako.template.Template("hern")
# ruleid:mako-templates-detected
template.Template("hern")
# ruleid:mako-templates-detected
Template("hello")

# ok
t = jinja2.Template("Hello {{ name }}")
t.render(name="world!")
