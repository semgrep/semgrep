from flask import Flask
from flask_talisman import Talisman, ALLOW_FROM

app = Flask(__name__)

talisman = Talisman(app)

@app.route('/test')
# ruleid: flask-talisman-decorator-cookie-secure-false
@talisman(session_cookie_secure=False, session_cookie_http_only=False)
def embeddable():
    return 'test'

@app.route('/test')
# ok: flask-talisman-decorator-cookie-secure-false
@talisman(session_cookie_secure=True, session_cookie_http_only=False)
def embeddable():
    return 'test'

@app.route('/test')
# ok: flask-talisman-decorator-cookie-secure-false
@talisman(session_cookie_http_only=False)
def embeddable():
    return 'test'
