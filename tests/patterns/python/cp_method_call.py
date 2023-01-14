# https://linear.app/r2c/issue/PA-588
def mk_query(user_name):
  query = "SELECT user_age FROM myapp_person where user_name = {}"
  # ERROR:
  return query.format(user_name)
