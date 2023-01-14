import flask
from flask import response as r

def foo():
   resp = r.set_cookie("sessionid", 
                       generate_cookie_value("RANDOM-UUID"), 
                       secure=True)
   a = set_cookie(1234, b, 123)

   if (x == y):
      r.set_cookie("sessionid")
      show_again()
      show()
   elif (x < y):
      do()
   else:
      donot()

   if "TOX_ENV_NAME" in os.environ:
       print("Not attempting to install binary while running under tox")
       return

   while a == b:
       do_again()
       if a: donot()

   for i in range(1, 10):
       run()
       print(i, tab=" ")
