array = [1, 'hi', 3.14]
array.each {|item| puts item }
# prints:
# 1
# 'hi'
# 3.14

array.each_index {|index| puts "#{index}: #{array[index]}" }
# prints:
# 0: 1
# 1: 'hi'
# 2: 3.14

# The following uses a (a..b) Range
(3..6).each {|num| puts num }
# prints:
# 3
# 4
# 5
# 6

# The following uses a (a...b) Range
(3...6).each {|num| puts num }
# prints:
# 3
# 4
# 5
