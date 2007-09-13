import os
from django.conf.urls.defaults import *
import rwh.comments.feeds as feeds

feeds = {
    'comments': feeds.Comments,
    }

urlpatterns = patterns('',
    (r'^comments/', include('rwh.comments.urls')),

    (r'^feeds/(?P<url>.*)/$', 'django.contrib.syndication.views.feed',
     {'feed_dict': feeds}),          

    # Only uncomment this for local testing without Apache.
    # (r'^html/(?P<path>.*)$', 'django.views.static.serve',
    # {'document_root': os.path.realpath(os.path.dirname(
    #    sys.modules[__name__].__file__) + '/../../en/html'),

    # Uncomment this for admin:
    (r'^admin/', include('django.contrib.admin.urls')),
)
