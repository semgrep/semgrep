import flask

app = flask.Flask(__name__)

@app.route("/route_param/<route_param>")
def route_param(route_param):
    print("blah")
    # ruleid: eval-injection
    return eval(route_param)

@app.route("/get_param", methods=["GET"])
def get_param():
    param = flask.request.args.get("param")
    # ruleid: eval-injection
    eval(param)

@app.route("/post_param", methods=["POST"])
def post_param():
    param = flask.request.form['param']
    if True:
        # ruleid: eval-injection
        eval(param)

@app.route("/format", methods=["POST"])
def format():
    param = "{}".format(flask.request.form['param'])
    print("do things")
    # ruleid: eval-injection
    eval(param)

@app.route("/ok")
def ok():
    eval("This is fine")

