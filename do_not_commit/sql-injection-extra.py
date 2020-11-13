from django.http import HttpResponse

class Person(models.Model):
    first_name = models.CharField(...)
    last_name = models.CharField(...)
    birth_date = models.DateField(...)

##### extra() True Positives #########
def get_user_age(request):
  # ruleid: sql-injection-using-extra-where
  user_name = request.data.get('user_name')
  user_age = Person.objects.extra(where=["name = %s" % user_name])
  html = "<html><body>User Age %s.</body></html>" % user_age
  return HttpResponse(html)

def get_user_age(request):
  # ruleid: sql-injection-using-extra-where
  user_name = request.data.get('user_name')
  user_age = Person.objects.extra(where=["name = %s" % user_name, "id not NULL"])
  html = "<html><body>User Age %s.</body></html>" % user_age
  return HttpResponse(html)

def get_user_age(request):
  # ruleid: sql-injection-using-extra-where
  path = request.path
  user_age = Person.objects.extra(where=["path = %s" % path])
  html = "<html><body>User Age %s.</body></html>" % user_age
  return HttpResponse(html)

def get_user_age(request):
  # ruleid: sql-injection-using-extra-where
  path = request.path
  user_age = Person.objects.extra(where=[f"path ={path}"])
  html = "<html><body>User Age %s.</body></html>" % user_age
  return HttpResponse(html)


##### extra() True Negative #########
def get_user_age(request):
  # no dataflow
  user_name = request.data.get('user_name')
  user_age = Person.objects.extra(where=["name = 'user_name'"])
  html = "<html><body>User Age %s.</body></html>" % user_age
  return HttpResponse(html)



