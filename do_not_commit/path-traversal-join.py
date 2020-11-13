from django.http import HttpResponse
import os

def foo_1(request):
  # ruleid: path-traversal-join
  param = request.GET.get('param')
  file_path = os.path.join("MY_SECRET", param)
  f = open(file_path, 'r')
  return HttpResponse(content=f, content_type="text/plain")

def foo_2(request):
  # ok due to abspath
  param = request.GET.get('param')
  file_path = os.path.join("MY_SECRET", param)
  file_path = os.path.abspath(file_path)
  f = open(file_path, 'r')
  return HttpResponse(content=f, content_type="text/plain")

def user_pic(request):
    """A view that is vulnerable to malicious file access."""

    base_path = os.path.join(os.path.dirname(__file__), '../../badguys/static/images')
    # ruleid: path-traversal-join
    filename = request.GET.get('p')

    data = open(os.path.join(base_path, filename), 'rb').read()

    return HttpResponse(data, content_type=mimetypes.guess_type(filename)[0])
