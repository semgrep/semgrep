from django.http import FileResponse

def func(request):
    # ruleid: request-data-fileresponse
    filename = request.POST.get("filename")
    f = open(filename, 'rb')
    return FileResponse(f)

def safe(request):
    # ok
    url = request.GET.get("url")
    print(url)
    f = open("blah.txt", 'r')
    return FileResponse(f)