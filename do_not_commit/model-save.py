from django.db import models
from django.template.defaultfilters import slugify

class Post(models.Model):
    title = models.CharField(max_length=255)
    slug = models.SlugField(unique=True, max_length=255)

    # ok
    def save(self, *args, **kwargs):
        if not self.slug:
            self.slug = slugify(self.title)
        super(Post, self).save(*args, **kwargs)

class Bad(models.Model):
    title = models.CharField(max_length=255)
    slug = models.SlugField(unique=True, max_length=255)

    # ruleid: django-db-model-save-super
    def save(self, *args, **kwargs):
        if not self.slug:
            self.slug = slugify(self.title)