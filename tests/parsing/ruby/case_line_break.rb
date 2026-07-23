# case with an optional line break before the guard expression
case foo
when bar
end

case
  foo
when bar
end

# case with guard and first when on the same line
case foo when bar
end

case foo when bar
  nil
end

# case_match with an optional line break before the guard expression
case expr
  in 5 then true
  else false
end

case
  expr
  in 5 then true
  else false
end

# case_match with guard clauses after a line break
case expr
  in x unless x < 0
  then true
  in x if x < 0
  then true
  else false
end
