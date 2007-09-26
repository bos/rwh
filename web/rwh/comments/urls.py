from django.conf.urls.defaults import *

urlpatterns = patterns('',
    (r'chapter/(?P<id>[^/]+)/?$', 'rwh.comments.views.chapter'),
    (r'chapter/(?P<id>[^/]+)/count/?$', 'rwh.comments.views.chapter_count'),
    (r'single/(?P<id>[^/]+)/?$', 'rwh.comments.views.single'),
    (r'submit/(?P<id>[^/]+)/?$', 'rwh.comments.views.submit')
)
