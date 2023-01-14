import django.db.models

from django.db import models

#ERROR: match
class A(models.Model):
      def save(self, *args, **kwargs):
          return self
