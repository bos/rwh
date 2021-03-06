import django.newforms as forms
from django.db import connection
from django.http import HttpResponse
from rwh.comments.models import Comment, Element
from django.shortcuts import get_object_or_404, render_to_response
from django.template import Context
from django.template.loader import get_template
from django.utils.simplejson import dumps 

def dump_queries():
    # requires settings.DEBUG to be set to True in order to work
    if len(connection.queries) == 1:
        print connection.queries
    else:
        qs = {}
        for q in connection.queries:
            qs[q['sql']] = qs.setdefault(q['sql'], 0) + 1
        for q in sorted(qs.items(), key=lambda x: x[1], reverse=True):
            print q
        print len(connection.queries)

class CommentForm(forms.Form):
    id = forms.CharField(widget=forms.HiddenInput)
    name = forms.CharField(max_length=64)
    url = forms.URLField(max_length=128, required=False)
    comment = forms.CharField(widget=forms.Textarea(attrs={
        'rows': 8, 'cols': 60
        }))
    remember = forms.BooleanField(initial=True, required=False)

def comments_by_chapter(id):
    objs = {}
    for c in Comment.objects.filter(element__chapter=id, hidden=False).order_by('date'):
        objs.setdefault(c.element_id, []).append(c)
    return objs

def chapter(request, id):
    template = get_template('comment.html')
    resp = {}
    for elt, comments in comments_by_chapter(id).iteritems():
        form = CommentForm(initial={
            'id': elt,
            'name': request.session.get('name', ''),
            })
        resp[elt] = template.render(Context({
            'id': elt,
            'form': form,
            'length': len(comments),
            'query': comments,
            }))
    return HttpResponse(dumps(resp), mimetype='application/json')

def chapter_count(request, id):
    resp = comments_by_chapter(id)
    for elt, comments in resp.iteritems():
        resp[elt] = len(comments)
    return HttpResponse(dumps(resp), mimetype='application/json')
    
def single(request, id, form=None, newid=None):
    queryset = Comment.objects.filter(element=id, hidden=False).order_by('date')
    if form is None:
        form = CommentForm(initial={
            'id': id,
            'name': request.session.get('name', ''),
            })
    try:
        error = form.errors[0]
    except:
        error = ''
    return render_to_response('comment.html', {
        'id': id,
        'form': form,
        'length': len(queryset),
        'query': queryset,
        'newid': newid or True,
        'error': error,
        })

def submit(request, id):
    element = get_object_or_404(Element, id=id)
    form = None
    newid = None
    if request.method == 'POST':
        form = CommentForm(request.POST)
        if form.is_valid():
            data = form.cleaned_data
            if data.get('remember'):
                request.session['name'] = data['name']
                request.session['url'] = data['url']
            else:
                request.session.pop('name', None)
                request.session.pop('url', None)
            c = Comment(element=element,
                        comment=data['comment'],
                        submitter_name=data['name'],
                        submitter_url=data['url'],
                        ip=request.META.get('REMOTE_ADDR'))
            c.save()
            newid = c.id
            form = None
    return single(request, id, form, newid)
