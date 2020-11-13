import base64
import mimetypes
import os

from django.core.urlresolvers import reverse
from django.http import HttpResponse
from django.shortcuts import redirect, render
from django.views.decorators.csrf import csrf_exempt

# adapted from https://github.com/mpirnat/lets-be-bad-guys/blob/7cbf11014bfc6dc9e199dc0b8a64e4597bc2338f/badguys/vulnerable/views.py#L95

def file_access(request):
    msg = request.GET.get('msg', '')
    # ok: context-autoescape-off
    return render(request, 'vulnerable/injection/file_access.html',
            {'msg': msg})

## 03 - XSS

def xss_form(request):
    # ruleid: context-autoescape-off
    env = {'qs': request.GET.get('qs', 'hello'), 'autoescape': False}
    response = render(request, 'vulnerable/xss/form.html', env)
    response.set_cookie(key='monster', value='omnomnomnomnom!')
    return response


def xss_path(request, path='default'):
    # ruleid: context-autoescape-off
    env = {'autoescape': False, 'path': path}
    return render(request, 'vulnerable/xss/path.html', env)


def xss_query(request):
    # ruleid: context-autoescape-off
    return render(request, 'vulnerable/xss/query.html', {'qs': request.GET.get('qs', 'hello'), "autoescape":False})