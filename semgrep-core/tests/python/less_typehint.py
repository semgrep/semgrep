from flask import send_file

app = Flask(__name__)

#ERROR: match
def download_not_flask_route(filename):
  return send_file(filename)

#ERROR: match
@app.route("/<path:filename>")
def download_file(filename: str):
  return send_file(filename)

