## SHOULD ALERT
import flask

## flask.send_file case
# ERROR: dangerous use of flask
flask.send_file(open("file.txt", "r"))

## With keyword args
# ERROR: dangerous use of flask
flask.send_file(open("file.txt", "r"), conditional=False)

# TODO: need name resolution in sgrep
## from flask import send_file case
# from flask import send_file
# send_file(open("file.txt", 'r'))
#
## Variable resolution
# from flask import send_file
# fin = open("file.txt", 'r')
# send_file(fin)
#
## Variable resolution with other statements
# from flask import send_file
# fin = open("file.txt", 'r')
# print("random statement")
# send_file(fin, conditional=False)

## SHOULD NOT ALERT

## Has a mime_type
flask.send_file(open("file.txt", "r"), mime_type="text/plain")

flask.send_file(open("file.txt", "r"), mime_type="text/plain", conditional=False)

# TODO: need name resolution in sgrep
## Has a attachment_filename
# from flask import send_file
# fin = open("file.txt", 'r')
# send_file(fin, as_attachment=True, attachment_filename="file.txt")
#
# from flask import send_file
# fin = open("file.txt", 'r')
# send_file(fin, as_attachment=True, attachment_filename="file.txt", conditional=False)
#

## String argument for arg0
flask.send_file("/tmp/file.txt")

import os

flask.send_file(os.path.join("data", "file.txt"))
