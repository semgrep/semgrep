import logging as logger
import traceback
from django.contrib.auth.models import User
from django.conf import settings
from django.core.mail import EmailMessage
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
            # ruleid: xss-html-email-body
            body_html = request.POST.get("body")

            sender = settings.EMAIL_SENDER

            email = EmailMessage(
                subject,
                body_html,
                sender,
                [settings.EMAIL],
                bcc=users,
            )
            email.content_subtype = "html"

            try:
                email.send()
                return render(
                    request,
                    "notification_email_conformation.html",
                    {"message": "All the emails are sent successfully!"},
                )
            except SMTPException:
                logger.exception(traceback.format_exc())
                return render(
                    request, "notification_email_data.html", {"errors": 1}
                )
        else:
            return render(request, "error404.html")
    else:
        return render(request, "error404.html")

def send_an_email(request):
    subject = request.POST.get("subject")
    # ok
    body_html = request.POST.get("body")

    sender = "blah@blah.com"
    email = EmailMessage(
        subject,
        body_html,
        sender,
        [settings.EMAIL],
    )

    try:
        email.send()
        return render(
            request,
            "notification_email_conformation.html",
            {"message": "All the emails are sent successfully!"},
        )
    except SMTPException:
        logger.exception(traceback.format_exc())
        return render(
            request, "notification_email_data.html", {"errors": 1}
        )