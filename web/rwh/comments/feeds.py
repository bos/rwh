from django.utils.feedgenerator import Atom1Feed
from django.contrib.syndication.feeds import Feed
from rwh.comments.models import Comment, Element

class AllComments(Feed):
    feed_type = Atom1Feed
    title = 'Real World Haskell: recent comments'
    subtitle = ('Recent comments on the text of &#8221;Real World '
                'Haskell&#8220;, from our readers')
    link = '/feeds/allcomments/'
    author_name = 'Our readers'

    def items(self):
        return Comment.objects.order_by('-date')[:20]

    def item_author_name(self, obj):
        return obj.submitter_name

    def item_pubdate(self, obj):
        return obj.date
