result =
  begin
   if 1 then a() end
  rescue Exception => x
   b()
  ensure
   c()
  end
