# cf. https://github.com/PyCQA/bandit/blob/02bad2e42311f420aef52dcd9806d66516ef594d/examples/jinja2_templating.py

import jinja2
from jinja2 import Environment, select_autoescape
templateLoader = jinja2.FileSystemLoader( searchpath="/" )
something = ''

# ok
Environment(loader=templateLoader, load=templateLoader, autoescape=True)

# ok
templateEnv = jinja2.Environment(autoescape=True,
        loader=templateLoader )

# ruleid:autoescape-disabled
Environment(loader=templateLoader, load=templateLoader, autoescape=something)


# ruleid:autoescape-disabled
templateEnv = jinja2.Environment(autoescape=False, loader=templateLoader )


# ruleid:autoescape-disabled
Environment(loader=templateLoader,
            load=templateLoader,
            autoescape=False)


# ruleid:autoescape-disabled
Environment(loader=templateLoader,
            load=templateLoader)

# ok
Environment(loader=templateLoader, autoescape=select_autoescape())

# ok
Environment(loader=templateLoader,
            autoescape=select_autoescape(['html', 'htm', 'xml']))


def fake_func():
    return 'foobar'
    

# ruleid:autoescape-disabled
Environment(loader=templateLoader, autoescape=fake_func())