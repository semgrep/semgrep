# ruleid:hardcoded-http-auth-in-controller
class DangerousController < ApplicationController

  http_basic_authenticate_with :name => "dhh", :password => "secret", :except => :index

  puts "do more stuff"

end

# ok
class OkController < ApplicationController

  http_basic_authenticate_with :name => "dhh", :password => not_a_string, :except => :index

  puts "do more stuff"

end
