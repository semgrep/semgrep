# https://stackoverflow.com/questions/5870537/whats-the-difference-between-django-onetoonefield-and-foreignkey/5891861#5891861

from django.db import models

class Engine(models.Model):
    name = models.CharField(max_length=25)

    def __unicode__(self):
        return self.name

class Car(models.Model):
    name = models.CharField(max_length=25)
    # ok
    engine = models.OneToOneField(Engine)

    def __unicode__(self):
        return self.name

class Engine2(models.Model):
    name = models.CharField(max_length=25)

    def __unicode__(self):
        return self.name

class Car2(models.Model):
    name = models.CharField(max_length=25)
    # ruleid: use-onetoonefield
    engine = models.ForeignKey(Engine2, unique=True, on_delete=models.CASCADE)

    def __unicode__(self):
        return self.name

