# cf. https://github.com/we45/Vulnerable-Flask-App/blob/752ee16087c0bfb79073f68802d907569a1f0df7/app/app.py

from flask import session, Flask, jsonify, request, Response, render_template, render_template_string, url_for
from flask_sqlalchemy import SQLAlchemy
import jwt
from jwt.exceptions import DecodeError, MissingRequiredClaimError, InvalidKeyError
import json
import random

app_port = os.environ.get('APP_PORT', 5050)
app = Flask(__name__, template_folder='templates')
db = SQLAlchemy(app)

@app.route('/search', methods = ['POST'])
def search_customer():
    token = request.headers.get('Authorization')
    if not token:
        return jsonify({'Error': 'Not Authenticated!'}),403
    else:
        if not verify_jwt(token):
            return jsonify({'Error': 'Invalid Token'}),403
        else:
            content = request.json
            results = []
            if content:
                try:
                    # ok
                    dummy = db.engine.execute("SELECT * FROM customer")

                    search_term = content['search']
                    # ruleid:formatted-sql-query
                    inline = db.engine.execute("SELECT * FROM cutsomer WHERE username = '%s'" % search_term)
                    print(search_term)
                    str_query = "SELECT first_name, last_name, username FROM customer WHERE username = '%s';".format(search_term)
                    # mycust = Customer.query.filter_by(username = search_term).first()
                    # return jsonify({'Customer': mycust.username, 'First Name': mycust.first_name}),200

                    # ruleid:formatted-sql-query
                    search_query = db.engine.execute(str_query)
                    for result in search_query:
                        results.append(list(result))
                    print(results)
                    return jsonify(results),200
                except Exception as e:
                    template = '''<html>
                        <head>
                        <title>Error</title>
                        </head>
                        <body>
                        <h1>Oops Error Occurred</h1>
                        <h3>%s</h3>
                        </body>
                        </html>
                        ''' % str(e)
                    return render_template_string(template, dir=dir, help=help, locals=locals), 404
