from django.shortcuts import redirect

def unsafe1(request):
    # ruleid: test-mvp-nested-1
    url = request.headers.get('referrer')
    print("something")
    return redirect(url)

def unsafe1(request):
    # ruleid: test-mvp-nested-1
    url = request.get('referrer')
    print("something")
    return redirect(url)

def unsafe2(request):
    # OK: test-mvp-nested-1
    url = request.get_full_path('referrer')
    print("something")
    return redirect(url)
