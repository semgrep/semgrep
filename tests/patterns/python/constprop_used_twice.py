import flask
from flask import response as r

app = flask.Flask()

somevar = True

@app.route("/admin")
def admin():
    # this cookie is secure
    # ERROR: match
    resp = r.set_cookie("sessionid", "RANDOM-UUID", secure=somevar, httponly=somevar, samesite="Lax")
    return resp
