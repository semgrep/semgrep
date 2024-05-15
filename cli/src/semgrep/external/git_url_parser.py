# This file is forked from https://github.com/coala/git-url-parse/blob/master/giturlparse/parser.py
# MIT license here: https://github.com/coala/git-url-parse/blob/master/LICENSE

# Copyright (c) 2017 John Dewey
#
#  Permission is hereby granted, free of charge, to any person obtaining a copy
#  of this software and associated documentation files (the "Software"), to
#  deal in the Software without restriction, including without limitation the
#  rights to use, copy, modify, merge, publish, distribute, sublicense, and/or
#  sell copies of the Software, and to permit persons to whom the Software is
#  furnished to do so, subject to the following conditions:
#
#  The above copyright notice and this permission notice shall be included in
#  all copies or substantial portions of the Software.
#
#  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
#  IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
#  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
#  AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
#  LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
#  FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
#  DEALINGS IN THE SOFTWARE.

#
# 2023-06-02 patched by Martin Jambon to avoid potential ReDoS attacks
# 2023-06-05 patched further by Martin Jambon to avoid potential ReDoS attacks
# 2023-10-12 patched by Zach Zeleznick to handle Azure URLs like
#   https://foobar.visualstudio.com/Data%20Classification/_git/Data%20Classification
#

import collections
import re
from typing import List
from typing import cast

Parsed = collections.namedtuple('Parsed', [
    'pathname',
    'protocols',
    'protocol',
    'href',
    'resource',
    'user',
    'port',
    'name',
    'owner',
    'azure_git_dir'
])

POSSIBLE_REGEXES = (
    re.compile(r'^(?P<protocol>https?|git|ssh|rsync)\://'
               r'(?:(?P<user>[^\n@]+)@)*'
               r'(?P<resource>[a-z0-9_\.-]*)'
               r'[:/]*'
               r'(?P<port>(?<=:)[\d]+){0,1}'
               r'(?P<pathname>\/((?P<owner>[\w\-%\/~\.]+)\/)?'
               # Matches the last name in a path (non-"/").
               # Trickily uses lazy matching "+?" to remove any
               # trailing ".git" and "/" from the end.
               r'((?P<name>[\w\-%~\.]+?)(\.git)?\/?)?)$'),
    re.compile(r'(git\+)?'
               r'((?P<protocol>\w+)://)'
               r'((?P<user>\w+)@)?'
               r'((?P<resource>[\w\.\-]+))'
               r'(:(?P<port>\d+))?'
               r'(?P<pathname>(\/(?P<owner>\w+)/)?'
               r'(\/?(?P<name>[\w\-]+)(\.git|\/)?)?)$'),
    re.compile(r'^'
               r'(?!\w+\://)'
               r'(?:(?P<user>[^\n@]+)@)*'
               r'(?P<resource>[a-z0-9_.-]*)[:]*'
               r'(?P<port>(?<=:)[\d]+){0,1}'
               r'(?P<pathname>\/?(?P<owner>.+)/(?P<name>.+?)(\.git)?\/?)$'),
    re.compile(r'((?P<user>\w+)@)?'
               r'((?P<resource>[\w\.\-]+))'
               r'[\:\/]{1,2}'
               r'(?P<pathname>((?P<owner>([\w\-]+\/)?\w+)/)?'
               r'((?P<name>[\w\-]+)(\.git|\/)?)?)$'),
    re.compile(r'((?P<user>\w+)@)?'
               r'((?P<resource>[\w\.\-]+))'
               r'[\:\/]{1,2}'
               r'(?P<pathname>((?P<owner>\w+)/)?'
               r'((?P<name>[\w\-\.]+)(\.git|\/)?)?)$'),
)


class ParserError(Exception):
    """ Error raised when a URL can't be parsed. """
    pass


class Parser(str):
    """
    A class responsible for parsing a GIT URL and return a `Parsed` object.
    """

    def __init__(self, url: str):
        # to fix an open bug with trailing slashes: https://github.com/coala/git-url-parse/issues/46
        self._url: str = url
        if url[-1] == "/":
          self._url = url[:-1]

    def parse(self) -> Parsed:
        """
        Parses a GIT URL and returns an object.  Raises an exception on invalid
        URL.
        :returns: Parsed object
        :raise: :class:`.ParserError`
        """
        d = {
            'pathname': None,
            'protocols': self._get_protocols(),
            'protocol': 'ssh',
            'href': self._url,
            'resource': None,
            'user': None,
            'port': None,
            'name': None,
            'owner': None,
            'azure_git_dir': ''
        }
        # Parsing is super slow even after fixing obvious problems in regexps.
        # This mitigates the damage of quadratic behavior.
        if len(self._url) > 1024:
            msg = f"URL exceeds maximum supported length of 1024: {self._url}"
            raise ParserError(msg)
        for regex in POSSIBLE_REGEXES:
            match = regex.search(self._url)
            if match:
                d.update(match.groupdict())
                break
        else:
            msg = "Invalid URL '{}'".format(self._url)
            raise ParserError(msg)

        if d['owner'] is not None and cast(str, d['owner']).endswith('/_git'):  # Azure DevOps Git URLs
            d['azure_git_dir'] = '/_git'
            d['owner'] = d['owner'][:-len('/_git')]

        return Parsed(**d)

    def _get_protocols(self) -> List[str]:
        try:
            index = self._url.index('://')
        except ValueError:
            return []

        return self._url[:index].split('+')
