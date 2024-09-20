from django.shortcuts import redirect
from django.http import HttpResponseRedirect, HttpResponse
from django.utils.http import is_safe_url

def unsafe(request):
    url = request.headers.get('referrer')
    print("something")
    #ruleid: tainting
    return redirect(url)

def safe(request):
    url = "https://lmnop.qrs"
    #OK:
    return redirect(url)

def unsafe2(request):
    url = request.POST.get("url")
    #ruleid: tainting
    return HttpResponseRedirect(url)

def unsafe3(request):
    url = request.POST["url"]
    #ruleid: tainting
    return HttpResponseRedirect(url)

def fine(request):
    #OK:
    return HttpResponseRedirect(request.get_full_path())

def url_validation(request):
    next = request.POST.get('next', request.GET.get('next'))
    if (next or not request.is_ajax()) and not is_safe_url(url=next, allowed_hosts=request.get_host()):
        next = "/index"
    #OK:
    response = HttpResponseRedirect(next) if next else HttpResponse(status=204)
    return response

def url_validation2(request):
    next = request.POST.get('next', request.GET.get('next'))
    ok = is_safe_url(url=next, allowed_hosts=request.get_host())
    if ok:
        #OK:
        response = HttpResponseRedirect(next) if next else HttpResponse(status=204)
    else:
        #OK:
        response = HttpResponseRedirect("index")
    return response
