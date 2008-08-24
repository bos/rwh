#!/usr/bin/env python
#
# This script updates the contents of the comments_element table.
# It's fugly, but a lot less painful than trying to use Django's
# fixtures system.

import os, sys
sys.path.append(os.path.dirname(__file__))
import dbutil

os.system('make -C ../../en ids')

conn = dbutil.connect()
c = conn.cursor()
c.execute('''load data local infile "../../en/all-ids.dat" replace
             into table comments_element
             fields terminated by "|"''')
print 'Database updated'
