class Person
end

# ruleid:ruby-eval
Person.class_eval do
  def say_hello
   "Hello!"
  end
end

jimmy = Person.new
jimmy.say_hello # "Hello!"

# ruleid:ruby-eval
Person.instance_eval do
  def human?
    true
  end
end

Person.human? # true

# ruleid:ruby-eval
Array.class_eval(array_second)



class Account < ActiveRecord::Base
  validates_format_of :name, :with => /^[a-zA-Z]+$/
  validates_format_of :blah, :with => /\A[a-zA-Z]+$/
  validates_format_of :something, :with => /[a-zA-Z]\z/
  validates_format_of :good_valid, :with => /\A[a-zA-Z]\z/ #No warning
  validates_format_of :not_bad, :with => /\A[a-zA-Z]\Z/ #No warning

  def mass_assign_it
    Account.new(params[:account_info]).some_other_method
  end

  def test_class_eval
    # ruleid:ruby-eval
    User.class_eval do
      attr_reader :some_private_thing
    end
  end
end

def zen
  41
end

# ruleid:ruby-eval
eval("def zen; 42; end")

puts zen

class Thing
end
a = %q{def hello() "Hello there!" end}
# ruleid:ruby-eval
Thing.module_eval(a)
puts Thing.new.hello()
