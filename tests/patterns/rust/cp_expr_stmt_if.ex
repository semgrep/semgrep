def expr_stmt_if(c) do
  x = if c do
        x = "not always this value"
      end
  # ok:
  x == "not always this value"
end
