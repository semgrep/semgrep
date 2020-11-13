class SignalsWeak:
    from django.dispatch.signals.Signal import disconnect
    # ruleid: django-compat-2_0-signals-weak
    disconnect(weak=True)
    # ok
    disconnect()

class CheckAggregateSupport:
    from django.db.backends.base.BaseDatabaseOperations import check_aggregate_support
    # ruleid: django-compat-2_0-check-aggregate-support
    check_aggregate_support()

class FormsExtras:
    # ruleid: django-compat-2_0-extra-forms
    from django.forms import extras

    # ruleid: django-compat-2_0-extra-forms
    from django.forms.extras import a

    # ruleid: django-compat-2_0-extra-forms
    from django.forms import extras as a

    # ruleid: django-compat-2_0-extra-forms
    from django.forms.extras import a as b

    # ruleid: django-compat-2_0-extra-forms
    import django.forms.extras

    # ruleid: django-compat-2_0-extra-forms
    import django.forms.extras as extras

    # ruleid: django-compat-2_0-extra-forms
    import django.forms.extras.a

    # ruleid: django-compat-2_0-extra-forms
    import django.forms.extras.a as b

class AssertRedirects:
    # ruleid: django-compat-2_0-assert-redirects-helper
    self.assertRedirects(expected_url="https://my.host/foo/bar", host="my.host")

    # ok
    self.assertRedirects(expected_url="https://my.host/foo/bar")

    # ruleid: django-compat-2_0-assert-redirects-helper
    assertRedirects(expected_url="https://my.host/foo/bar", host="my.host")

    # ok
    assertRedirects(expected_url="https://my.host/foo/bar")

class AssignmentHelper:
    # ruleid: django-compat-2_0-assignment-tag
    Library().assignment_tag(settings)

    # ruleid: django-compat-2_0-assignment-tag
    assignment_tag(settings)
