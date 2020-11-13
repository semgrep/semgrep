# ruleid:mass-assignment-protection-disabled
User.new(params[:user], :without_protection => true)

# ok
User.new(params[:user])
