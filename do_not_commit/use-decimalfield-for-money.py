from django.db import models
from django.utils import timezone

# Create your models here.

class Product(models.Model):
    title = models.CharField(max_length=64)
    description = models.TextField()
    # ok:use-decimalfield-for-money
    price = models.DecimalField(max_digits=6, decimal_places=2)
    # ruleid:use-decimalfield-for-money
    old_price = models.FloatField()
    image = models.CharField(max_length=256)
    stock = models.IntegerField(null=True)
    date_added = models.DateTimeField(default=timezone.now)

class User(models.Model):
    email = models.EmailField()
    password = models.CharField(max_length=32)

    gender = models.BooleanField()
    first_name = models.CharField(max_length=32)
    last_name = models.CharField(max_length=32)

    postal_code = models.CharField(max_length=16)
    house_number_additions = models.CharField(max_length=16)
    street_name = models.CharField(max_length=64)
    city_name = models.CharField(max_length=32)
    country = models.CharField(max_length=32)

    phone_number = models.CharField(max_length=16)
    date_of_birth = models.DateField()
