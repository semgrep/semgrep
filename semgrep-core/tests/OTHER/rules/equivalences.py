def route(request):
   # ruleid: equivalences-example
   username = request.POST["username"]
   # ruleid: equivalences-example
   password = request.POST.get("password")
   return auth(username, password)
