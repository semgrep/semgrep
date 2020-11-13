from flask import request

app = Flask(__name__)


@app.route('/', method="GET")
def handler_with_get_json(ff):
  # ruleid:avoid-accessing-request-in-wrong-handler
  r = request.json
  return r

@app.route('/', method="GET")
def handler_with_get_form(ff):
  # ruleid:avoid-accessing-request-in-wrong-handler
  r = request.form
  return r

@app.route('/', method="GET")
def handler_with_data(ff):
  # ruleid:avoid-accessing-request-in-wrong-handler
  r = request.data
  return r
