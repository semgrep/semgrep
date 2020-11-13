import flask
import json

app = flask.Flask(__name__)

@app.route("/route_param/<route_param>")
def route_param(route_param):
    print("blah")
    # ruleid: path-traversal-open
    return open(route_param, 'r').read()

@app.route("/get_param", methods=["GET"])
def get_param():
    param = flask.request.args.get("param")
    # ruleid: path-traversal-open
    f = open(param, 'w')
    f.write("hello world")

@app.route("/post_param", methods=["POST"])
def post_param():
    param = flask.request.form['param']
    if True:
        # ruleid: path-traversal-open
        with open(param, 'r') as fin:
            data = json.load(fin)
    return data

@app.route("/subexpression", methods=["POST"])
def subexpression():
    param = "{}".format(flask.request.form['param'])
    print("do things")
    # ruleid: path-traversal-open
    return open(param, 'r').read()

@app.route("/ok")
def ok():
    # ok
    open("static/path.txt", 'r')

