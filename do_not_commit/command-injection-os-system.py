import os

def danger(request):
    # ruleid: command-injection-os-system
    url = request.GET['url']
    os.system('wget ' + url)

def danger2(request):
    # ruleid: command-injection-os-system
    image = request.POST['image']
    os.system("./face-recognize %s --N 24" % image)

def danger3(request):
    # ruleid: command-injection-os-system
    url = request.GET['url']
    os.system("nslookup " + url)

def ok(request):
    # ok
    url = request.GET['url']
    os.system("echo 'hello'")