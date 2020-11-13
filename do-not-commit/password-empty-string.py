import os
from typing import Any
from typing import Dict
from typing import List

import ujson
from backend import EmailAuthBackend
from django.http import HttpRequest
from django.http import HttpResponse
from django.shortcuts import render
from django.test import Client
from models import UserProfile

from tests import example_user


def test_email_auth_backend_empty_password(user_profile: UserProfile) -> None:
    user_profile = example_user("hamlet")
    # ok: password-empty-string
    password = "testpassword"
    user_profile.set_password(password)
    user_profile.save()

    # First, verify authentication works with the a nonempty
    # password so we know we've set up the test correctly.
    self.assertIsNotNone(
        EmailAuthBackend().authenticate(
            username=self.example_email("hamlet"), password=password
        )
    )

    # Now do the same test with the empty string as the password.
    # ruleid: password-empty-string
    password = ""
    user_profile.set_password(password)
    user_profile.save()
    self.assertIsNone(
        EmailAuthBackend().authenticate(
            username=self.example_email("hamlet"), password=password
        )
    )

    # Now do the same test with the empty string as the password.
    # ruleid: password-empty-string
    password = ""
    user_profile.set_password(password)
    user_profile.save()
    self.assertIsNone(
        EmailAuthBackend().authenticate(
            username=self.example_email("hamlet"), password=password
        )
    )
