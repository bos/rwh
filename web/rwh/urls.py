from django.conf.urls.defaults import *
import rwh.comments.feeds as feeds

feeds = {
    'comments': feeds.Comments,
    }

urlpatterns = patterns('',
    (r'^comments/', include('rwh.comments.urls')),

    (r'^feeds/(?P<url>.*)/$', 'django.contrib.syndication.views.feed',
     {'feed_dict': feeds}),          

    (r'^html/(?P<path>.*)$', 'django.views.static.serve',
     {'document_root': '/home/bos/src/darcs/book/en/html'}),

    # Uncomment this for admin:
    (r'^admin/', include('django.contrib.admin.urls')),
)
