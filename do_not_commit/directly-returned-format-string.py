# -*- coding: utf-8 -*-
import os
import sqlite3

from flask import Flask
from flask import redirect
from flask import request
from flask import session
from jinja2 import Template

app = Flask(__name__)

@app.route("/loginpage")
def render_login_page():
    thing = "blah"
    # ruleid:directly-returned-format-string
    return '''
<p>{}</p>
<form method="POST" style="margin: 60px auto; width: 140px;">
    <p><input name="username" type="text" /></p>
    <p><input name="password" type="password" /></p>
    <p><input value="Login" type="submit" /></p>
</form>
    '''.format(thing)

@app.route("/loginpage2")
def render_login_page2():
    thing = "blah"
    # ruleid:directly-returned-format-string
    return '''
<p>%s</p>
<form method="POST" style="margin: 60px auto; width: 140px;">
    <p><input name="username" type="text" /></p>
    <p><input name="password" type="password" /></p>
    <p><input value="Login" type="submit" /></p>
</form>
    ''' % thing

@app.route("/loginpage3")
def render_login_page3():
    thing = "blah"
    # ruleid:directly-returned-format-string
    return '''
<p>%s</p>
<form method="POST" style="margin: 60px auto; width: 140px;">
    <p><input name="username" type="text" /></p>
    <p><input name="password" type="password" /></p>
    <p><input value="Login" type="submit" /></p>
</form>
    ''' % (thing,)
    
@app.route("/loginpage4")
def render_login_page4():
    thing = "blah"
    # ruleid:directly-returned-format-string
    return thing + '''
<form method="POST" style="margin: 60px auto; width: 140px;">
    <p><input name="username" type="text" /></p>
    <p><input name="password" type="password" /></p>
    <p><input value="Login" type="submit" /></p>
</form>
    '''
    
@app.route("/loginpage5")
def render_login_page5():
    thing = "blah"
    # ruleid:directly-returned-format-string
    return f'''
{thing}
<form method="POST" style="margin: 60px auto; width: 140px;">
    <p><input name="username" type="text" /></p>
    <p><input name="password" type="password" /></p>
    <p><input value="Login" type="submit" /></p>
</form>
    '''

if __name__ == '__main__':
    app.run(debug=True)
