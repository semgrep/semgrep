import bcrypt
from app import User

def handle_signup(username, password):
    return User.create(username, bcrypt.hashpw(password, bcrypt.getsalt()))

def get_user(user_id):
    return User.query.filter_by(user_id=user_id).first()