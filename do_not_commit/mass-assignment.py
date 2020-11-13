from django.shortcuts import render
from myapp.models import Whatzit

# Test cases borrowed from https://gist.github.com/jsocol/3217262

def create_whatzit(request):
    # ruleid: mass-assignment
    Whatzit.objects.create(**request.POST)
    return render(request, 'created.html')

def update_whatzit(request, id):
    whatzit = Whatzit.objects.filter(pk=id)
    # ruleid: mass-assignment
    whatzit.update(**request.POST)
    whatzit.save()
    return render(request, 'saved.html')

def good_whatzit(request):
    # ok
    Whatzit.objects.create(
        name=request.POST.get('name'),
        dob=request.POST.get('dob')
    )
    return render(request, 'created.html')
