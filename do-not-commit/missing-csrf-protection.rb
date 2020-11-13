# ruleid:missing-csrf-protection
class DangerousController < ActionController::Base

  puts "do more stuff"

end

# ok
class OkController < ActionController::Base

  protect_from_forgery :with => :exception

  puts "do more stuff"

end
