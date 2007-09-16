#!/usr/bin/env python
#
# Use Django's template machinery to expand static web pages.  First
# tries the default template path for a particular installation, then
# looks for templates in the filesystem.

from django.template import Context, TemplateDoesNotExist
from django.template.loader import get_template, get_template_from_string
from django.core.management import setup_environ
import rwh.settings as settings
import sys

setup_environ(settings)
c = Context()

if len(sys.argv) == 2:
    in_name = sys.argv[1]
    out_name = 'stdout'
    out_fp = sys.stdout
elif len(sys.argv) == 3:
    in_name = sys.argv[1]
    out_name = sys.argv[2]
    out_fp = None
else:
    print >> sys.stderr, 'Usage: %s template-file [output-file]'
    sys.exit(1)
    
try:
    t = get_template(in_name)
except TemplateDoesNotExist:
    t = get_template_from_string(open(in_name).read(), name=in_name)
if out_fp is None:
    out_fp = open(out_name, 'w')
out_fp.write(t.render(c))
out_fp.close()
