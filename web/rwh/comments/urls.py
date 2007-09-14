from django.conf.urls.defaults import *

urlpatterns = patterns('',
    (r'chapter/(?P<id>[^/]+)/?$', 'rwh.comments.views.chapter'),
    (r'single/(?P<id>[^/]+)/?$', 'rwh.comments.views.single'),
    (r'submit/(?P<id>[^/]+)/?$', 'rwh.comments.views.submit')
)
