import os
from typing import Any
from typing import Dict
from typing import List

import ujson
from backend import EmailAuthBackend
from django.contrib.auth import get_user_model
from django.contrib.auth.password_validation import validate_password
from django.core.exceptions import ValidationError
from django.http import HttpRequest
from django.http import HttpResponse
from django.http import HttpResponseBadRequest
from django.shortcuts import render
from django.test import Client
from models import UserProfile
from somewhere import BaseBackend

from tests import example_user

UserModel = get_user_model()


def test_email_auth_backend_empty_password(user_profile: UserProfile) -> None:
    user_profile = example_user("hamlet")
    password = "testpassword"
    try:
        validate_password(password)
    except ValidationError as e:
        return HttpResponseBadRequest(str(e))
        # ok
    user_profile.set_password(password)
    user_profile.save()

    user_profile.assertIsNotNone(
        EmailAuthBackend().authenticate(
            username=user_profile.example_email("hamlet"), password=password
        )
    )


def other(user_profile: UserProfile) -> None:
    user_profile = example_user("hamlet")
    password = "testpassword"
    # ruleid: unvalidated-password
    user_profile.set_password(password)

    user_profile.save()

    user_profile.assertIsNotNone(
        EmailAuthBackend().authenticate(
            username=user_profile.example_email("hamlet"), password=password
        )
    )


class ModelBackend(BaseBackend):
    """
    Authenticates against settings.AUTH_USER_MODEL.
    """

    def authenticate(self, request, username=None, password=None, **kwargs):
        if username is None:
            username = kwargs.get(UserModel.USERNAME_FIELD)
        if username is None or password is None:
            return
        try:
            user = UserModel._default_manager.get_by_natural_key(username)
        except UserModel.DoesNotExist:
            # Run the default password hasher once to reduce the timing
            # difference between an existing and a nonexistent user (#20760).
            # ok
            UserModel().set_password(password)
        else:
            if user.check_password(password) and self.user_can_authenticate(user):
                return user
