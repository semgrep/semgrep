from flask import Flask

app = Flask(__name__)

# ruleid: flask-duplicate-handler-name
@app.route('/hello')
def hello():
    return 'hello'

@app.route('/hi', methods=["POST"])
def hello():
  return 'hi'

# ok
@app.route('/howdy/:name')
def howdy(name):
  return f"""howdy {name}"""