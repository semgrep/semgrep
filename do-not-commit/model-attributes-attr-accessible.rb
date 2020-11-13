class User < ActiveRecord::Base
acts_as_authentic do |t|
    t.login_field=:login # for available options see documentation in: Authlogic::ActsAsAuthentic
  end # block optional
	attr_accessible :login
  attr_accessible :first_name
	attr_accessible :middle_name
	attr_accessible :surname
	attr_accessible :permanent_address
	attr_accessible :correspondence_address
	attr_accessible :email
	attr_accessible :contact_no
	attr_accessible :gender
	attr_accessible :password
	attr_accessible :password_confirmation
	attr_accessible :avatar
	has_attached_file :avatar, :styles => { :medium => "300x300>", :thumb => "100x100>" }
end

def create
    user = User.create(person_params)
end

# ruleid: model-attributes-attr-accessible
class User < ActiveRecord::Base
acts_as_authentic do |t|
    t.login_field=:login # for available options see documentation in: Authlogic::ActsAsAuthentic
  end # block optional
	has_attached_file :avatar, :styles => { :medium => "300x300>", :thumb => "100x100>" }
end

def create
    user = User.create(person_params)
end
