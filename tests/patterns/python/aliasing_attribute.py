import django.views.decorators.csrf.csrf_exempt

from django.views.decorators.csrf import csrf_exempt

#ERROR: match
@csrf_exempt
def foo():
  return 1

#ERROR: match
@django.views.decorators.csrf.csrf_exempt
def foo():
  return 1
