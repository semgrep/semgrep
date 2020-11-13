import flask

app = flask.Flask(__name__)

@app.route("/route_param/<route_param>")
def route_param(route_param):
    print("blah")
    # ruleid: exec-injection
    return exec(route_param)

@app.route("/get_param", methods=["GET"])
def get_param():
    param = flask.request.args.get("param")
    # ruleid: exec-injection
    exec(param)

@app.route("/post_param", methods=["POST"])
def post_param():
    param = flask.request.form['param']
    if True:
        # ruleid: exec-injection
        exec(param)

@app.route("/subexpression", methods=["POST"])
def subexpression():
    param = "{}".format(flask.request.form['param'])
    print("do things")
    # ruleid: exec-injection
    exec(param)

@app.route("/ok")
def ok():
    exec("This is fine")

