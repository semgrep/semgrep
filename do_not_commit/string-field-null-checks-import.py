from django.db import models
from django.db.models import Model, CharField, IntegerField, TextField

class FakeModel(Model):
    # ok
    fieldOne = CharField(
        max_length=200,
        unique=True)
    # ruleid: string-field-must-set-null-true
    fieldTwo = CharField(
        unique=True,
        blank=True,
        max_length=30
    )
    # ruleid: string-field-must-set-null-true
    fieldTwo = TextField(
        unique=True,
        blank=True,
        max_length=30
    )
    # ok
    fieldThree = CharField(
        unique=True,
        null=True,
        blank=True,
        max_length=100
    )
    # ok
    notText = IntegerField(
        max_value=255
    )

def fake(**kwargs):
    pass

def nope():
    # ok
    return fake(unique=True, blank=True)
