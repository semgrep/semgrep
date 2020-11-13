import flask
from flask import response as r

app = flask.Flask(__name__)
# ruleid:flask-wtf-csrf-disabled
app.config['WTF_CSRF_ENABLED'] = False

@app.route("/index")
def index():
    return 'hello world'