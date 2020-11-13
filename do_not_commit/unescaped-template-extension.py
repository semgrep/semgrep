from flask import Flask, render_template
app = Flask(__name__)

@app.route("/unsafe")
def unsafe():
    # ruleid: unescaped-template-extension
    return render_template("unsafe.txt", name=request.args.get("name"))

@app.route("/really_unsafe")
def really_unsafe():
    name = request.args.get("name")
    age = request.args.get("age")
    # ruleid: unescaped-template-extension
    return render_template("unsafe.txt", name=name, age=age)

@app.route("/no_extension")
def no_extension():
    # ruleid: unescaped-template-extension
    return render_template("will-crash-without-extension", name=request.args.get("name"))

# Test a bunch at the same time
evil = "<script>alert('blah')</script>"

@app.route("/one")
def one():
    # ruleid: unescaped-template-extension
    return render_template("unsafe.unsafe", name=evil)

@app.route("/two")
def two():
    # ruleid: unescaped-template-extension
    return render_template("unsafe.email", name=evil)

@app.route("/three")
def three():
    # ruleid: unescaped-template-extension
    return render_template("unsafe.jinja2", name=evil)

@app.route("/four")
def four():
    # ruleid: unescaped-template-extension
    return render_template("unsafe.template", name=evil)

@app.route("/five")
def five():
    # ruleid: unescaped-template-extension
    return render_template("unsafe.asdlfkjasdlkjf", name=evil)

@app.route("/six")
def six():
    # ruleid: unescaped-template-extension
    return render_template("unsafe.html.j2", name=evil)

@app.route("no_vars")
def no_vars():
    # ok
    return render_template("unsafe.txt")

@app.route("/escaped_extensions")
def escaped_extensions():
    # ok
    return render_template("safe.html", name=request.args.get("name"))
    
@app.route("/concat")
def concat():
    # ruleid: unescaped-template-extension
    msg.body = render_template(template + '.txt', **kwargs)
    # ok
    msg.html = render_template(template + '.html', **kwargs)
    # ruleid: unescaped-template-extension
    return render_template('%s.txt' % style, **kwargs).replace('<table>', table)
    
@app.route("/format")
def format():
    name = "world"
    # ruleid: unescaped-template-extension
    return render_template("{}.txt".format("hello"), name)

@app.route("/format-ok")
def format():
    name = "world"
    # ok
    return render_template("{}.html".format("hello"), name)

from library import render_template
def not_flask():
    from library import render_template
    # ok
    return render_template("hello.txt")

@app.route("/what_if")
def what_if():
    cond = request.args.get("cond")
    if cond:
        template = "unsafe.txt"
    else:
        template = "safe.html"
    return render_template(template, cond=cond)

# Real-world code
@app.route("/opml")
def opml():
    sort_key = flask.request.args.get("sort", "(unread > 0) DESC, snr")
    if sort_key == "feed_title":
        sort_key = "lower(feed_title)"
    order = flask.request.args.get("order", "DESC")
    with dbop.db() as db:
        rows = dbop.opml(db)
        return (
            # ruleid: unescaped-template-extension
            flask.render_template("opml.opml", atom_content=atom_content, rows=rows),
            200,
            {"Content-Type": "text/plain"},
        )