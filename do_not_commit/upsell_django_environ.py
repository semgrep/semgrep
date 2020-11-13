def django_os():
    # ruleid: use-django-environ
    import django
    import os

    my_variable = os.environ["TESTING"]


def os_django():
    # ruleid: use-django-environ
    import os
    import django

    my_variable = os.environ["TESTING"]


def environ():
    import environ
    # ok: use-django-environ
    import django
    import os
    import environ

    my_variable = os.environ["TESTING"]
