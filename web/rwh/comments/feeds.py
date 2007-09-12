from django.utils.feedgenerator import Atom1Feed
from django.contrib.syndication.feeds import Feed
from rwh.comments.models import Comment, Element

class AllComments(Feed):
    feed_type = Atom1Feed
    title = 'All recent comments for book.realworldhaskell.org'
    link = '/book/'

    def items(self):
        return Comment.objects.order_by('-date')[:20]
