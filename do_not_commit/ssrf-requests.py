import flask
import requests

app = flask.Flask(__name__)

@app.route("/route_param/<route_param>")
def route_param(route_param):
    print("blah")
    # ruleid: ssrf-requests
    return requests.get(route_param)

@app.route("/get_param", methods=["GET"])
def get_param():
    param = flask.request.args.get("param")
    # ruleid: ssrf-requests
    requests.post(param, timeout=10)

@app.route("/post_param", methods=["POST"])
def post_param():
    param = flask.request.form['param']
    if True:
        # ruleid: ssrf-requests
        requests.get(param)

@app.route("/subexpression", methods=["POST"])
def subexpression():
    param = "{}".format(flask.request.form['param'])
    print("do things")
    # ruleid: ssrf-requests
    requests.post(param, data={"hello", "world"})

@app.route("/ok")
def ok():
    requests.get("https://www.google.com")

