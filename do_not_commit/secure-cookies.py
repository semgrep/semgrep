from django.shortcuts import render

from django.conf import settings
from django.contrib.messages.storage.base import BaseStorage, Message

class CookieStorage(BaseStorage):
    def _update_cookie(self, encoded_data, response):
        """
        Either sets the cookie with the encoded data if there is any data to
        store, or deletes the cookie.
        """
        if encoded_data:
            # ok
            response.set_cookie(
                self.cookie_name, encoded_data,
                domain=settings.SESSION_COOKIE_DOMAIN,
                secure=settings.SESSION_COOKIE_SECURE or None,
                httponly=settings.SESSION_COOKIE_HTTPONLY or None,
            )
        else:
            response.delete_cookie(self.cookie_name, domain=settings.SESSION_COOKIE_DOMAIN)


def index(request, template):
    response = render(request, template)

    # ok
    response.set_cookie("hello", "world", secure=True, httponly=True, samesite="Lax")

    # ok
    response.set_cookie("hello", "world", **kwargs)

    # ok
    response.set_cookie(
        settings.SESSION_COOKIE_NAME,
        request.session.session_key, max_age=max_age,
        expires=expires, domain=settings.SESSION_COOKIE_DOMAIN,
        path=settings.SESSION_COOKIE_PATH,
        secure=settings.SESSION_COOKIE_SECURE or None,
        httponly=settings.SESSION_COOKIE_HTTPONLY or None,
    )

    # ok
    response.set_cookie(
        settings.CSRF_COOKIE_NAME,
        request.META['CSRF_COOKIE'],
        max_age=settings.CSRF_COOKIE_AGE,
        domain=settings.CSRF_COOKIE_DOMAIN,
        path=settings.CSRF_COOKIE_PATH,
        secure=settings.CSRF_COOKIE_SECURE,
        httponly=settings.CSRF_COOKIE_HTTPONLY,
    )

    # ruleid: django-secure-set-cookie
    response.set_cookie("hello", "again", httponly=False)

    return response
