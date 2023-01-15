from metrics import send
def foo():
   bar()
   bar + send('my-report-id') + bar()
