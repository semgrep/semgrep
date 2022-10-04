#ERROR: match
"Do you like cats?" =~ /like/

#ERROR: match
if "Do you like cats?".match(/like/)
  puts "Match found!"
end
