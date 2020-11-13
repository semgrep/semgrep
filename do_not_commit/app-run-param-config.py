#ruleid:avoid_app_run_with_bad_host
app.run(host="0.0.0.0")

#ruleid:avoid_app_run_with_bad_host
app.run("0.0.0.0")

# OK
foo.run("0.0.0.0")
