from django.db import models
from django.shortcuts import get_object_or_404

mutable = True

class Element(models.Model):
    class Admin:
        search_fields = ['id', 'chapter']
        list_filter = ['chapter']

    id = models.CharField(max_length=64, editable=False, primary_key=True)
    chapter = models.CharField(max_length=64, editable=False, db_index=True)

    def __unicode__(self):
        return self.id
    
class Comment(models.Model):
    class Admin:
        search_fields = ['comment']
        date_hierarchy = 'date'
        list_filter = ['date']

    element = models.ForeignKey(Element)
    comment = models.TextField(editable=mutable)
    submitter_name = models.CharField(max_length=64)
    submitter_url = models.URLField(blank=True, editable=mutable)
    ip = models.IPAddressField(editable=mutable)
    date = models.DateTimeField('date submitted', auto_now=True,
                                auto_now_add=True)
    reviewed = models.BooleanField()

    def __unicode__(self):
        return self.comment[:32]

    def get_absolute_url(self):
        return '/html/%s.html#%s' % (self.element.chapter, self.element.id)
