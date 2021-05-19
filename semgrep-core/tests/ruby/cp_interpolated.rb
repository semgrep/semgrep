age = "42"
query = "SELECT name FROM users WHERE age=#{age}"
# ERROR: match
foo(query)
# ERROR: match
foo("DELETE FROM table WHERE age=#{age}")

def bar
  q = "SELECT name FROM users WHERE age=#{age}"
  # ERROR: match
  foo(q)
end
