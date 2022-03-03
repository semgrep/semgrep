from flask import Flask, request, render_template
from flask_sqlalchemy import SQLAlchemy
import bcrypt
from deeper import handle_signup
from deep import indirection1

app = Flask(__name__)
db = SQLAlchemy(app)

class User(db.Model):
    user_id = db.Column(db.Integer, primary_key=True)
    username = db.Column(db.String(90), unique=True)
    password = db.Column(db.String(90))

@app.route('/signup', methods=['POST'])
def signup():
    body = request.json
    if body:
        username = body['username']
        password = body['password']
        handle_signup(username, password)
        #User.create(username, bcrypt.hashpw(password, bcrypt.getsalt()))
        return "OK", 200
    return "ERROR", 404

@app.route('/user/<user_id>', methods=['GET'])
def user(user_id):
    user = indirection1(user_id=user_id)
    # ruleid: stored-xss
    return render_template("user.html.j2", username=user.username)

