import django.newforms as forms
from django.http import HttpResponse
from rwh.comments.models import Comment, Element
from django.shortcuts import get_object_or_404, render_to_response
from django.template import Context
from django.template.loader import get_template
from django.utils.simplejson import dumps 

class CommentForm(forms.Form):
    id = forms.CharField(widget=forms.HiddenInput)
    name = forms.CharField(max_length=64)
    url = forms.URLField(max_length=128, required=False)
    comment = forms.CharField(widget=forms.Textarea(attrs={
        'rows': 8, 'cols': 60
        }))
    remember = forms.BooleanField(initial=True, required=False)

def chapter(request, id):
    template = get_template('comment.html')
    objs = {}
    for c in Comment.objects.filter(element__chapter=id).order_by('date'):
        objs.setdefault(c.element.id, [])
        objs[c.element.id].append(c)
    resp = {}
    for elt, comments in objs.iteritems():
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

def single(request, id, form=None, newid=None):
    queryset = Comment.objects.filter(element=id).order_by('date')
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
