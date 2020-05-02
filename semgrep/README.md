# Running Tox Locally

If you want to run tox tests locally. This is a mild pain -- I don't expect people to need to do this regularly but instead just rely on the Github Action that runs tox.
1. `brew install pyenv`
2. `pyenv install 3.6.6 3.7.7 3.8.2` 
3. `pyenv global 3.6.6 3.7.7 3.8.2`
4. `python3 virtualenv .venv`
5. `.venv/bin/activate`
6. `pip install tox` or `pip install -r requirements.txt`
7. `tox`


