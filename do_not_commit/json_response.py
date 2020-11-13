from django.http import HttpResponse
import json

def foo():
    # ruleid:use-json-response
    dump = json.dumps({})
    return HttpResponse(dump, content_type='application/json')

def foo1():
    # ruleid:use-json-response
    dump = json.dumps({})
    x = HttpResponse(dump, content_type='application/json')