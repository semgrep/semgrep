from django.db import models
from django.db.models import Model

class FakeModel(Model):
    #ruleid: no-null-string-field
    fieldOne = models.CharField(
        max_length=200,
        null=True)
    # ruleid: string-field-must-set-null-true
    fieldTwo = models.CharField(
        unique=True,
        blank=True,
        max_length=30
    )
    # ruleid: string-field-must-set-null-true
    fieldAAAAA = models.TextField(
        unique=True,
        blank=True,
        max_length=30
    )
    # ok
    fieldThree = models.CharField(
        unique=True,
        null=True,
        blank=True,
        max_length=100
    )
    # ok
    notText = models.IntegerField(
        max_value=255
    )

def fake(**kwargs):
    pass

def nope():
    # ok
    return fake(unique=True, blank=True)
