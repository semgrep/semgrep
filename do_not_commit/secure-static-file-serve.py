from flask import send_file

app = Flask(__name__)

@app.route("/<path:filename>")
def download_file(filename):
  # ruleid:avoid_send_file_without_path_sanitization
  return send_file(filename)

def download_not_flask_route(filename):
  # ok
  return send_file(filename)
