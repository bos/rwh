from django.db import models
from django.shortcuts import get_object_or_404

mutable = True

class Element(models.Model):
    class Admin:
        search_fields = ['id', 'section']
        list_filter = ['section', 'title']

    id = models.CharField('ID attribute', max_length=64, editable=False,
                          primary_key=True)
    section = models.CharField('Section ID', max_length=64, editable=False,
                               db_index=True)
    title = models.CharField('Section title', max_length=256, editable=False)

    def __unicode__(self):
        return self.id
    
class Comment(models.Model):
    class Admin:
        search_fields = ['comment']
        date_hierarchy = 'date'
        list_filter = ['date']

    element = models.ForeignKey(Element,
        help_text='ID of paragraph that was commented on')
    comment = models.TextField(editable=mutable,
        help_text='Text of submitted comment (do not modify)')
    submitter_name = models.CharField('Submitter', max_length=64,
        help_text='Self-reported name of submitter (may be bogus)')
    submitter_url = models.URLField('URL', blank=True, editable=mutable,
        help_text='Self-reported URL of submitter (may be empty or bogus)')
    ip = models.IPAddressField('IP address', editable=mutable,
        help_text='IP address from which comment was submitted')
    date = models.DateTimeField('date submitted', auto_now=True,
                                auto_now_add=True)
    reviewed = models.BooleanField(
        help_text='Has this comment been reviewed by an author?')
    hidden = models.BooleanField(default=False, db_index=True,
        help_text='Has this comment been hidden from public display?')

    def __unicode__(self):
        return self.comment[:32]

    def get_absolute_url(self):
        return '/html/%s.html#%s' % (self.element.section, self.element.id)
