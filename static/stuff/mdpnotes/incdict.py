# -*- coding: utf-8 -*-

#*****************************************************************************
#       Copyright (C) 2009 Carlo Hamalainen <carlo.hamalainen@gmail.com>, 
#
#  Distributed under the terms of the GNU General Public License (GPL)
#
#    This code is distributed in the hope that it will be useful,
#    but WITHOUT ANY WARRANTY; without even the implied warranty of
#    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
#    General Public License for more details.
#
#  The full text of the GPL is available at:
#
#                  http://www.gnu.org/licenses/
#*****************************************************************************

class IncDict(dict):
    """
    For code where we update a count in a dictionary, we often need this
    kind of thing to avoid throwing an exception:

    d = {}
    try: d['a'] += 1
    except KeyError: d['a'] = 0 

    The IncDict class makes __getitem__ return 0 for any key that is
    not in the dictionary, and so we can do this instead:

    >>> d = IncDict()
    >>> print d
    {}
    >>> d['a'] += 1
    >>> print d
    {'a': 1}
    >>> d['a'] += 1
    >>> print d
    {'a': 2}
    """

    def __getitem__(self, y):
        if self.has_key(y): return dict.__getitem__(self, y)
        else: return 0
