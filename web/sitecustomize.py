# There's already a system-wide Django instance on the server I'm
# using.  It's both old and installed using easy_install, so it
# pollutes the front of sys.path.
#
# I say we take off and nuke the entire site from orbit. It's the only
# way to be sure.

import sys

sys.path.remove('/usr/local/src/django_src')
