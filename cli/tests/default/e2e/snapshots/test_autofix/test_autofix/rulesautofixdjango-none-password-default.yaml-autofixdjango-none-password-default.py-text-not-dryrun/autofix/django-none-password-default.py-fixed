from django.contrib import auth
from django.contrib.auth.password_validation import validate_password
from django.core.exceptions import PermissionDenied, ValidationError
from django.utils.translation import gettext as _
from django.views.decorators.csrf import csrf_protect
from rest_framework import status
from rest_framework.decorators import api_view, permission_classes
from rest_framework.response import Response

from ...conf import settings
from ...core.decorators import require_dict_data
from ...core.mail import mail_user
from ..bans import get_user_ban
from ..forms.auth import AuthenticationForm, ResendActivationForm, ResetPasswordForm
from ..serializers import AnonymousUserSerializer, AuthenticatedUserSerializer
from ..tokens import (
    is_password_change_token_valid,
    make_activation_token,
    make_password_change_token,
)
from .rest_permissions import UnbannedAnonOnly, UnbannedOnly

User = auth.get_user_model()
BaseUserManager = User.__class__

class PasswordChangeFailed(Exception):
    pass

def change_forgotten_password(request, pk, token):
    """
    POST /auth/change-password/user/token/ with CSRF and new password
    will change forgotten password
    """
    if request.settings.enable_sso:
        raise PermissionDenied(_("Please use the 3rd party site to authenticate."))

    invalid_message = _("Form link is invalid. Please try again.")
    expired_message = _("Your link has expired. Please request new one.")

    try:
        try:
            user = User.objects.get(pk=pk, is_active=True)
        except User.DoesNotExist:
            raise PasswordChangeFailed(invalid_message)

        if request.user.is_authenticated and request.user.id != user.id:
            raise PasswordChangeFailed(invalid_message)
        if not is_password_change_token_valid(user, token):
            raise PasswordChangeFailed(invalid_message)

        if user.requires_activation:
            raise PasswordChangeFailed(expired_message)
        if get_user_ban(user, request.cache_versions):
            raise PasswordChangeFailed(expired_message)
    except PasswordChangeFailed as e:
        return Response({"detail": e.args[0]}, status=status.HTTP_400_BAD_REQUEST)

    try:
        # ruleid: use-none-for-password-default
        new_password = request.data.get("password", None)
        validate_password(new_password, user=user)
        user.set_password(new_password)
        user.save()
    except ValidationError as e:
        return Response({"detail": e.messages[0]}, status=status.HTTP_400_BAD_REQUEST)

    return Response({"username": user.username})

class UserManager(BaseUserManager):
    # ruleid: use-none-for-password-default
    def create_user(self, email, password=None):
        """
        Creates and saves a Poster with the given email and password.
        """
        if not email:
            raise ValueError('Users must have an email address')

        user = self.model(email=self.normalize_email(email))
        user.set_password(password)
        user.save(using=self._db)
        return user
