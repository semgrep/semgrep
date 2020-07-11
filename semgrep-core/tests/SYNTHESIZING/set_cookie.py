import flask
from flask import response as r

def foo():
   resp = r.set_cookie("sessionid", 
                       generate_cookie_value("RANDOM-UUID"), 
                       secure=True)
   a = set_cookie(1234, b, 123)

   if "TOX_ENV_NAME" in os.environ:
       print("Not attempting to install binary while running under tox")
       return
