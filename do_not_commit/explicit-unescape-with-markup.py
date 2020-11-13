from flask import render_template, Markup, request
from markupsafe import Markup as mkup

from application import app

@app.route('/markup')
def markup_test():
    search_query = request.args.get('q')
    if search_query:
        search_query = '"{0}"'.format(
            search_query.replace('\"', '\\\"').strip())
    else:
        search_query = '""'

    playlist = request.args.get('p')
    if playlist:
        playlist = '"{0}"'.format(playlist.replace('\"', '\\\"').strip())
    else:
        playlist = '""'
    # ruleid: explicit-unescape-with-markup
    return render_template('/markup.html', query=Markup(search_query), playlist=Markup(playlist))

@app.route('/markup_unescape')
def markup_unescape_test():
    search_query = request.args.get('q')
    # ruleid: explicit-unescape-with-markup
    return render_template('/markup-unescape.html', query=Markup.unescape(search_query))

@app.route('/markupsafe')
def markupsafe_test():
    search_query = request.args.get('q')
    # ruleid: explicit-unescape-with-markup
    return render_template('/markup-unescape.html', query=mkup(search_query))

@app.route('/good')
def good_test():
    search_query = request.args.get('q')
    # ok
    return render_template('/markup-unescape.html', query=Markup.escape(search_query))

