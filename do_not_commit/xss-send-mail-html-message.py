import logging as logger
import traceback
from django.contrib.auth.models import User
from django.conf import settings
from django.core.mail import send_mail
from django.shortcuts import render

from smtplib import SMTPException

def notify_users_about_challenge(request):
    """
    Email New Challenge Details to Users
    """
    if request.user.is_authenticated() and request.user.is_superuser:
        if request.method == "GET":
            template_name = "notification_email_data.html"
            return render(request, template_name)

        elif request.method == "POST":
            users = User.objects.exclude(email__exact="").values_list(
                "email", flat=True
            )
            subject = request.POST.get("subject")
            body = request.POST.get("body")
            # ruleid: xss-send-mail-html-message
            body_html = request.POST.get("body_html")

            sender = settings.EMAIL_SENDER

            send_mail(
                subject,
                body,
                sender,
                [settings.EMAIL],
                bcc=users,
                html_message=body_html
            )
        else:
            return render(request, "error404.html")
    else:
        return render(request, "error404.html")

def send_an_email(request):
    subject = request.POST.get("subject")
    # ok
    body= request.POST.get("body")

    sender = "blah@blah.com"
    send_mail(
        subject,
        body,
        sender,
        [settings.EMAIL],
    )