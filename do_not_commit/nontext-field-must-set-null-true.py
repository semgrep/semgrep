from django.db import models


class FakeModel(models.Model):
    # ok
    fieldChar = models.CharField(
        max_length=200,
        blank=True)
    # ok
    fieldText = models.TextField(blank=True)
    # ok
    fieldSlug = models.SlugField(blank=True)
    # ok
    fieldEmail = models.EmailField(blank=True)
    # ok
    fieldURL = models.URLField(blank=True)
    # ok
    fieldUUID = models.UUIDField(blank=True)
    # ok
    fieldManyToMany = models.ManyToManyField("self", blank=True)
    # ruleid: nontext-field-must-set-null-true
    fieldInt = models.IntegerField(
        blank=True,
        max_value=30
    )
    # ok
    fieldIntNull = models.IntegerField(
        null=True,
        blank=True,
        max_value=100
    )

def fake(**kwargs):
    pass

def nope():
    # ok
    return fake(blank=True)
