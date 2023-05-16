# MATCH
begin
    result = 10 / 0 # division by zero
rescue Exception => e
    puts "handled" # code to handle the exception
end