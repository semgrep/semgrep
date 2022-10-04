# this used to not parse with tree-sitter
class A
  #ERROR: match
  INCLUDE = ->(list, action) { list.include? action }
  #ERROR: match
  EXCLUDE = ->(list, action) { !list.include? action }
end
