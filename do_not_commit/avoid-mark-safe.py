from django.utils.safestring import mark_safe
from django.utils.html import format_html
from django.http import HttpResponse
from django.template import loader

def not_really_safe(request):
    template = loader.get_template('contents.html')
    # ruleid:avoid-mark-safe
    not_actually_safe = mark_safe(
        """
        <div>
            <p>Contents! %s</p>
        </div>
        """ % request.POST.get("contents")
    )
    return HttpResponse(template.render({"html_example": not_actually_safe}, request))

def fine(request):
    template = loader.get_template('contents.html')
    # ok
    fine = mark_safe(
        """
        <div>
            <p>Contents!</p>
        </div>
        """
    )
    return HttpResponse(template.render({"html_example": fine}, request))

def not_really_safe(request):
    template = loader.get_template('contents.html')
    # ok
    this_is_ok = format_html(
        """
        <div>
            <p>Contents! {}</p>
        </div>
        """,
        request.POST.get("contents")
    )
    return HttpResponse(template.render({"html_example": this_is_ok}, request))
