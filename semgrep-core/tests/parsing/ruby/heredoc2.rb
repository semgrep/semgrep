type  = "healthy"
table = "food"
query = <<-SQL
SELECT * FROM #{table}
WHERE #{type} = true
SQL
