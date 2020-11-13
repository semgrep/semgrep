def test1():
    import flask
    response = flask.make_response()
    # ruleid:secure-set-cookie
    response.set_cookie("cookie_name", "cookie_value")
    return response

def test2():
    from flask import make_response
    r = make_response()
    # some values are set but not others

    # ruleid:secure-set-cookie
    r.set_cookie("cookie1", "cookie_value", secure=True)
    # ruleid:secure-set-cookie
    r.set_cookie("cookie2", "cookie_value", httponly=True)
    # ruleid:secure-set-cookie
    r.set_cookie("cookie3", "cookie_value", samesite="Lax")
    # ruleid:secure-set-cookie
    r.set_cookie("cookie4", "cookie_value", secure=True, httponly=True)
    # ruleid:secure-set-cookie
    r.set_cookie("cookie5", "cookie_value", httponly=True, samesite="Lax")

def test3():
    import flask
    response = flask.make_response()
    # all present
    # ok
    response.set_cookie("cookie1", "cookie_value", secure=True, httponly=True, samesite='Lax')
    # ok
    response.set_cookie("cookie2", "cookie_value", secure=True, httponly=True, samesite='Strict')
    # ok
    response.set_cookie("cookie3", "cookie_value", secure=False, httponly=False, samesite=None)

# ok
def set_cookie(settings):
    d = {"hello": "world"}
    d.update(settings)
    return d

def use_cookie(cookie):
    # ok
    foo = set_cookie({"goodbye": "planet"})
    
# cf. # https://github.com/pallets/flask/blob/b7f6fae9b34341b9be7742b86f6caffe07fc6f25/tests/test_basic.py#L1956
def test_real_code():
    import flask
    app = flask.Flask(__name__)
    @app.route("/")
    def index():
        r = flask.Response("", status=204)
        # ruleid: secure-set-cookie
        r.set_cookie("foo", "bar" * 100)
        return r

# cf. https://github.com/cruzegoodin/TSC-ShippingDetails/blob/cceee79014623c5ac8fb042b8301a427743627d6/venv/lib/python2.7/site-packages/pip/_vendor/requests/cookies.py#L306
import copy
import time
import collections
from .compat import cookielib, urlparse, urlunparse, Morsel
def merge_cookies(cookiejar, cookies):
    if not isinstance(cookiejar, cookielib.CookieJar):
        raise ValueError('You can only merge into CookieJar')
    if isinstance(cookies, dict):
        cookiejar = cookiejar_from_dict(
            cookies, cookiejar=cookiejar, overwrite=False)
    elif isinstance(cookies, cookielib.CookieJar):
        try:
            cookiejar.update(cookies)
        except AttributeError:
            for cookie_in_jar in cookies:
                # ok
                cookiejar.set_cookie(cookie_in_jar)
    return cookiejar