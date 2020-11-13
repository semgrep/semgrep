def test_bad_1():
    from requests import get
    from django.shortcuts import render

    def send_to_redis(request):
        # ruleid: ssrf-injection-requests
        bucket = request.GET.get("bucket")
        inner_response = get("http://my.redis.foo/{}".format(bucket), data=3)
        return render({"response_code": inner_response.status_code})

def test_bad_2():
    from requests import get
    from django.http import HttpResponse

    def send_to_redis(request):
        # ruleid: ssrf-injection-requests
        bucket = request.GET.get("bucket")
        inner_response = get("http://my.redis.foo/{}".format(bucket), data=3)
        return HttpResponse(body = {"response_code": inner_response.status_code})

def test_bad_3():
    from requests import get
    from django.shortcuts import render

    def send_to_redis(request):
        # ruleid: ssrf-injection-requests
        bucket = request.GET.get("bucket")
        inner_response = get(f"http://my.redis.foo/{bucket}", data=3)
        return render({"response_code": inner_response.status_code})

def test_bad_4():
    from requests import get
    from django.shortcuts import render

    def send_to_redis(request):
        # ruleid: ssrf-injection-requests
        bucket = request.headers.get("bucket")
        inner_response = get("http://my.redis.foo/{}".format(bucket), data=3)
        return render({"response_code": inner_response.status_code})

def test_bad_5():
    from requests import get
    from django.shortcuts import render

    def send_to_redis(request):
        # ruleid: ssrf-injection-requests
        bucket = request.GET["bucket"]
        inner_response = get("http://my.redis.foo/{}".format(bucket), data=3)
        return render({"response_code": inner_response.status_code})

def test_bad_6():
    from requests import get
    from django.shortcuts import render

    def send_to_redis(request):
        # ruleid: ssrf-injection-requests
        bucket = request.headers["bucket"]
        inner_response = get("http://my.redis.foo/{}".format(bucket), data=3)
        return render({"response_code": inner_response.status_code})

