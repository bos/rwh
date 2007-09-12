from django.conf.urls.defaults import *

urlpatterns = patterns('',
    (r'single/(?P<id>[^/]+)/?$', 'rwh.comments.views.single'),
    (r'submit/(?P<id>[^/]+)/?$', 'rwh.comments.views.submit')
)
