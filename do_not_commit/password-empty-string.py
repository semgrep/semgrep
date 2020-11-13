import os
import ujson
from typing import Any, Dict, List

from django.http import HttpRequest, HttpResponse
from django.shortcuts import render
from django.test import Client

from tests import example_user
from models import UserProfile
from backend import EmailAuthBackend

def test_email_auth_backend_empty_password(user_profile: UserProfile) -> None:
	user_profile = example_user('hamlet')
    # ok: password-empty-string
	password = "testpassword"
	user_profile.set_password(password)
	user_profile.save()

	# First, verify authentication works with the a nonempty
	# password so we know we've set up the test correctly.
	self.assertIsNotNone(EmailAuthBackend().authenticate(username=self.example_email('hamlet'), password=password))

	# Now do the same test with the empty string as the password.
    # ruleid: password-empty-string
	password = ""
	user_profile.set_password(password)
	user_profile.save()
	self.assertIsNone(EmailAuthBackend().authenticate(username=self.example_email('hamlet'), password=password))

	# Now do the same test with the empty string as the password.
    # ruleid: password-empty-string
	password = ''
	user_profile.set_password(password)
	user_profile.save()
	self.assertIsNone(EmailAuthBackend().authenticate(username=self.example_email('hamlet'), password=password))
