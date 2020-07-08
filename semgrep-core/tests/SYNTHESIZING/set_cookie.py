import flask
from flask import response as r

def foo():
   resp = r.set_cookie("sessionid", 
                       generate_cookie_value("RANDOM-UUID"), 
                       secure=True)
   resp = r.set_cookie("sessionid", resp, "RANDOM-UUID")
