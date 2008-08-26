#!/usr/bin/env python
# -*- coding: utf-8 -*-

import os, sys
sys.path.append(os.path.dirname(__file__))
import dbutil

conn = dbutil.connect()
c = conn.cursor()

c.execute('''select submitter_name from comments_comment''')

reviewers = {}

mappings = {
    u'alejandro "tab-lover" dubrovsky': u'Alejandro Dubrovsky',
    u'alex hirzel <ahirzel@mtu.edu>': u'Alex Hirzel',
    u'anonymous coward': u'Anonymous',
    u'arthur van leeuwen': u'Arthur van Leeuwen',
    u'augustss': u'Lennart Augustsson',
    u'ed t': u'Anonymous',
    u'geogre moschovitis': u'George Moschovitis',
    u'george m': u'George Moschovitis',
    u'haskell newb': u'Anonymous',
    u'j. pablo fernandez': u'J. Pablo Fernández',
    u'kamal al-marhoobi': u'Kamal Al-Marhubi',
    u'kevin w.': u'Kevin Watters',
    u'max cantor (#haskell - mxc)': u'Max Cantor',
    u'michael campbell': u'Michael Campbell',
    u'mike btauwerman': u'Mike Brauwerman',
    u'no credit necessary': u'Anonymous',
    u'nykänen, matti': u'Matti Nykänen',
    u'omar antolin camarena': u'Omar Antolín Camarena',
    u'ryan t mulligan': u'Ryan T. Mulligan',
    u'sengan baring-gould': u'Sengan Baring-Gould',
    u'some guy': u'Anonymous',
    u'tomas janousek': u'Tomáš Janoušek',
    u'william halchin': u'William N. Halchin',
    }

def fixup(s):
    try:
        return s.encode('ascii')
    except UnicodeEncodeError:
        def f(c):
            o = ord(c)
            if o < 128:
                return c
            return '&#%d;' % o
        return ''.join(map(f, s))

total = 0
for r in c.fetchall():
    r = r[0].decode('utf-8')
    if r in ("Bryan O'Sullivan", 'John Goerzen', 'Don Stewart'):
        continue
    total += 1
    m = mappings.get(r.lower())
    if m:
        r = m
    elif len(r) < 2 or ' ' not in r:
        r = 'Anonymous'
    reviewers.setdefault(r, 0)
    reviewers[r] += 1

reviewers = sorted(reviewers.iteritems(), key=lambda x: x[0])

cohorts = [(.01,1),(.002,.01)]

for (lo,hi) in cohorts:
    lo = total * lo
    hi = total * hi
    for r in [n for n in reviewers if lo <= n[1] < hi]:
        if r[1] > 3:
            print '%s,' % fixup(r[0])
    print

lo = total * .002
for n in reviewers:
    if n[1] < lo:
        print '%s,' % fixup(n[0])
