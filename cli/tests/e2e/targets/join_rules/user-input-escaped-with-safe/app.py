from flask import (
    Flask,
    render_template,
    request,
)
from database import db
import os


app = Flask(__name__)

app.secret_key = os.urandom(16)
app.config["SQLALCHEMY_TRACK_MODIFICATIONS"] = False
app.config["SQLALCHEMY_DATABASE_URI"] = "sqlite:///db.sqlite3"

db.init_app(app)
with app.app_context():
    db.create_all()

@app.route("/search")
def search():
    query = request.args.get("q")
    if query is None:
        search_results = []
    else:
        query = query.strip().lower()
        searchtest = db.session.execute("SELECT * FROM products WHERE lower(product_name) LIKE '%{}%'".format(query))
        search_results = [i for i in searchtest]
        # search_results = [i for i in Product.query.all() if query in i.product_name.lower()]

    return render_template("search.html", query=query, search_results=search_results)


if __name__ == "__main__":
    app.run(debug=True)
