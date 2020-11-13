from django.http import HttpResponse

class Person(models.Model):
    first_name = models.CharField(...)
    last_name = models.CharField(...)
    birth_date = models.DateField(...)

##### raw() True Positives #########
def get_user_age(request):
  # ruleid: sql-injection-using-raw
  user_name = request.get('user_name')
  user_age = Person.objects.raw('SELECT user_age FROM myapp_person where user_name = %s' % user_name)
  html = "<html><body>User Age %s.</body></html>" % user_age
  return HttpResponse(html)

def get_user_age(request):
  # ruleid: sql-injection-using-raw
  user_name = request.get('user_name')
  user_age = Person.objects.raw(f"SELECT user_age FROM myapp_person where user_name = {user_name}")
  html = "<html><body>User Age %s.</body></html>" % user_age
  return HttpResponse(html)

def get_user_age(request):
  # ruleid: sql-injection-using-raw
  user_name = request.get('user_name')
  user_age = Person.objects.raw('SELECT user_age FROM myapp_person where user_name = %s'.format(user_name))
  html = "<html><body>User Age %s.</body></html>" % user_age
  return HttpResponse(html)

def get_users(request):
  # ruleid: sql-injection-using-raw
  client_id = request.headers.get('client_id')
  users = Person.objects.raw('SELECT * FROM myapp_person where client_id = %s' % client_id)
  html = "<html><body>Users %s.</body></html>" % users
  return HttpResponse(html)

def get_users(request):
  # ruleid: sql-injection-using-raw
  client_id = request.headers.get('client_id')
  users = Person.objects.raw(f'SELECT * FROM myapp_person where client_id = {client_id}')
  html = "<html><body>Users %s.</body></html>" % users
  return HttpResponse(html)

##### raw() True Negatives #########
def get_user_age(request):
  user_name = request.get('user_name')
  # django queryset is good
  user_age = Person.objects.filter(user_name=user_name).first()
  html = "<html><body>User Age %s.</body></html>" % user_age
  return HttpResponse(html)

def get_users(request):
  client_id = request.headers.get('client_id')
  # using param list is ok
  users = Person.objects.raw('SELECT * FROM myapp_person where client_id = %s', (client_id,))
  html = "<html><body>Users %s.</body></html>" % users
  return HttpResponse(html)
