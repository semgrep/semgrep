hash = Hash.new # equivalent to hash = {}
hash = { :water => 'wet', :fire => 'hot' } # makes the previous line redundant as we are now
                                           # assigning hash to a new, separate hash object
puts hash[:fire] # prints "hot"

hash.each_pair do |key, value|   # or: hash.each do |key, value|
  puts "#{key} is #{value}"
end
# returns {:water=>"wet", :fire=>"hot"} and prints:
# water is wet
# fire is hot

hash.delete :water                            # deletes the pair :water => 'wet' and returns "wet"
hash.delete_if {|key,value| value == 'hot'}   # deletes the pair :fire => 'hot' and returns {}
