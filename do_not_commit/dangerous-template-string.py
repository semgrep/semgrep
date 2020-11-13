import flask

app = flask.Flask(__name__)

@app.route("/error")
def error(e):
    # ruleid: dangerous-template-string
    template = '''{  extends "layout.html"  }
{  block body  }
    <div class="center-content error">
        <h1>Oops! That page doesn't exist.</h1>
        <h3>%s</h3>
    </div>
{  endblock  }
'''.format(request.url)
    return flask.render_template_string(template), 404

@app.route("/error2")
def error2(e):
    # ruleid: dangerous-template-string
    template = '''{  extends "layout.html"  }
{  block body  }
    <div class="center-content error">
        <h1>Oops! That page doesn't exist.</h1>
        <h3>%s</h3>
    </div>
{  endblock  }
''' % (request.url)
    return flask.render_template_string(template), 404

## Doesn't work yet
#@app.route("/error3")
#def error3(e):
#    template = f'''{  extends "layout.html"  }
#{  block body  }
#    <div class="center-content error">
#        <h1>Oops! That page doesn't exist.</h1>
#        <h3>{request.url}</h3>
#    </div>
#{  endblock  }
#'''
#    return flask.render_template_string(template)

@app.route("/error4")
def error4(e):
    # ruleid: dangerous-template-string
    template = """
{  extends "layout.html"  }
{  block body  }
    <div class="center-content error">
        <h1>Oops! That page doesn't exist.</h1>
        <h3>
"""
    template += request.url
    template += """
</h3>
</div>
{  endblock  }
"""
    rendered = flask.render_template_string(template)
