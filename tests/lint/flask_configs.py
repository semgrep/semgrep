import os

from flask import Flask

app = Flask(__name__)

# SHOULD MATCH

# For `TESTING`
# ERROR:
app.config["TESTING"] = True
# ERROR:
app.config["TESTING"] = False
# ERROR:
app.config.update(TESTING=True)

# For `SECRET_KEY`
# ERROR:
app.config.update(SECRET_KEY="aaaa")
# ERROR:
app.config["SECRET_KEY"] = b'_5#y2L"F4Q8z\n\xec]/'

# For `ENV`
# ERROR:
app.config["ENV"] = "development"
# ERROR:
app.config["ENV"] = "production"

# For `DEBUG`
# ERROR:
app.config["DEBUG"] = True
# ERROR:
app.config["DEBUG"] = False

# SHOULD NOT MATCH
# For `TESTING`
app.config["TESTING"] = os.getenv("TESTING")
app.config["TESTING"] = "aa"

# For `SECRET_KEY`
app.config.update(SECRET_KEY=os.getenv("SECRET_KEY"))
app.config.update(SECRET_KEY=os.environ["SECRET_KEY"])

# FOR `ENV`
app.config["ENV"] = os.environ["development"]

# For `DEBUG`
app.config["DEBUG"] = os.environ["DEBUG"] or True
app.config["DEBUG"] = os.environ["DEBUG"] or False
