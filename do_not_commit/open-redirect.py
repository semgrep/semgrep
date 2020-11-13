from flask import Flask, redirect, request, url_for
from werkzeug.urls import url_parse

app = Flask(__name__)

@app.route("open_redirect/")
def open_redirect():
    # ruleid: open-redirect
    url = request.args.get("url")
    print("something")
    return redirect(url)

@app.route("not_open_redirect/")
def not_open_redirect():
    # ok
    url = "/about/"
    return redirect(url)

@app.route("filter")
def filter():
    # ok
    next_page = request.args.get('next')
    if not next_page or url_parse(next_page).netloc != '':
        next_page = url_for('main.index')
    return redirect(next_page)

from flask import request, redirect

# cf. https://github.com/mideind/Netskrafl/blob/2e1933ad0710a4425c319fde3b92b2a70729ed80/netskrafl.py#L1712
@app.route("/userprefs", methods=["GET", "POST"])
@auth_required()
def userprefs():
    """ Handler for the user preferences page """

    user = current_user()

    uf = UserForm()
    err = dict()

    # The URL to go back to, if not main.html
    # ruleid: open-redirect
    from_url = request.args.get("from", None)

    if request.method == "GET":
        # Entering the form for the first time: load the user data
        uf.init_from_user(user)
    elif request.method == "POST":
        # Attempting to submit modified data: retrieve it and validate
        uf.init_from_form(request.form)
        err = uf.validate()
        if not err:
            # All is fine: store the data back in the user entity
            uf.store(user)
            return redirect(from_url or url_for("main"))

    # Render the form with the current data and error messages, if any
    return render_template("userprefs.html", uf=uf, err=err, from_url=from_url)
