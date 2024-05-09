from django.http import HttpResponse
from django.http import HttpResponseRedirect
from django.shortcuts import redirect
from django.utils.http import is_safe_url


def arg(request):
    # ruleid: open-redirect
    return redirect(request.POST.get("next"))


def argh(request):
    # ruleid: open-redirect
    return redirect(request.get_host())


def arghh(request):
    # ruleid: open-redirect
    return redirect(request.method)


def argh2(request):
    # ruleid: open-redirect
    url = request.get_host()
    print("something")
    return redirect(url)


def unsafe(request):
    # ruleid: open-redirect
    url = request.headers.get("referrer")
    print("something")
    return redirect(url)


def safe(request):
    # ok
    url = "https://lmnop.qrs"
    return redirect(url)


def fine(request):
    # ok
    return HttpResponseRedirect("https://google.com")


def unsafe2(request):
    # ruleid: open-redirect
    url = request.POST.get("url")
    return HttpResponseRedirect(url)


def legit(request):
    # ok
    return HttpResponseRedirect(request.get_full_path())


def url_validation(request):
    # ok
    next = request.POST.get("next", request.GET.get("next"))
    if (next or not request.is_ajax()) and not is_safe_url(
        url=next, allowed_hosts=request.get_host()
    ):
        next = "/index"
    response = HttpResponseRedirect(next) if next else HttpResponse(status=204)
    return response


def url_validation2(request):
    # ok
    next = request.POST.get("next", request.GET.get("next"))
    ok = is_safe_url(url=next, allowed_hosts=request.get_host())
    if ok:
        response = HttpResponseRedirect(next) if next else HttpResponse(status=204)
    else:
        response = HttpResponseRedirect("index")
    return response
