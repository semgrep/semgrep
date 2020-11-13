def mass_assign_unsafe
    #ruleid: mass-assignment-vuln
    User.new(params[:user])
    #ruleid: mass-assignment-vuln
    user = User.new(params[:user])
    #ruleid: mass-assignment-vuln
    User.new(params[:user], :without_protection => true)
end

def safe_send
    #ok
    attr_accessible :name
    User.new(params[:user])

    #ok
    attr_accessible :name
    user = User.new(params[:user])
end
