parsy
=====

|Documentation Status| |Build Status| |Codecov| |Downloads|

Parsy is an easy and elegant way to parse text in Python by combining small
parsers into complex, larger parsers. If it means anything to you, it's a
monadic parser combinator library for LL(infinity) grammars in the spirit of
`Parsec <https://github.com/haskell/parsec>`_, `Parsnip
<http://parsnip-parser.sourceforge.net/>`_, and `Parsimmon
<https://github.com/jneen/parsimmon>`_. But don't worry, it has really good
documentation and it doesn't say things like that!

Parsy requires Python 3.7 or greater.

For a good example of the kind of clear, declarative code you can create using
parsy, see the `SQL SELECT statement example
<https://parsy.readthedocs.io/en/latest/howto/other_examples.html#sql-select-statement-parser>`_
or `JSON parser
<https://parsy.readthedocs.io/en/latest/howto/other_examples.html#json-parser>`_.

Links:

- `Documentation <http://parsy.readthedocs.io/en/latest/>`_
- `History and changelog <http://parsy.readthedocs.io/en/latest/history.html>`_
- `PyPI <https://pypi.python.org/pypi/parsy/>`_

To contribute, please create a fork and submit a pull request on GitHub, after
checking the `contributing
<https://parsy.readthedocs.io/en/latest/contributing.html>`_ section of the
docs. Thanks!

If you like parsy and think it should be better known, you could:

* Star this project on GitHub.
* `Vote <https://github.com/vinta/awesome-python/pull/993>`_ for it being included on awesome-python.

Parsy was originally written by `Jeanine Adkisson <https://github.com/jneen>`_,
with contributions by other people as can be found in the git commit history.

.. |Documentation Status| image:: https://readthedocs.org/projects/parsy/badge/?version=latest
   :target: http://parsy.readthedocs.io/en/latest/?badge=latest
.. |Build Status| image:: https://img.shields.io/github/workflow/status/python-parsy/parsy/Tests
   :target: https://github.com/python-parsy/parsy/actions?query=workflow%3A%22Tests%22+branch%3Amaster
.. |Codecov| image:: https://img.shields.io/codecov/c/github/python-parsy/parsy/master.svg
   :target: https://codecov.io/gh/python-parsy/parsy
.. |Downloads| image:: https://img.shields.io/pypi/dm/parsy
   :target: https://pypi.org/project/parsy/
