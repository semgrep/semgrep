from flask import Flask, json_available, request, testing

app = Flask(__name__)

# ruleid: flask-deprecated-apis
if json_available:
    pass

# ruleid: flask-deprecated-apis
blueprint = request.module

# ruleid: flask-deprecated-apis
builder = testing.make_test_environ_builder(app)

# ruleid: flask-deprecated-apis
app.open_session(...)

# ruleid: flask-deprecated-apis
app.save_session(...)

# ruleid: flask-deprecated-apis
app.make_null_session(...)

# ruleid: flask-deprecated-apis
app.init_jinja_globals(...)

# ruleid: flask-deprecated-apis
app.request_globals_class(...)

# ruleid: flask-deprecated-apis
app.static_path(...)

# ruleid: flask-deprecated-apis
app.config.from_json(...)


@app.route("/foo")
def foo():
    pass


if request.method == "POST":
    pass

app.config["BAR"] = "BAZ"
app.register_blueprint(blueprint=object())
