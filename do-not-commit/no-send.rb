def bad_send
    # ruleid: bad-send
    method = params[:method]
    @result = User.send(method.to_sym)
end

def ok_send
    method = params[:method] == 1 ? :method_a : :method_b
    @result = User.send(method, *args)
end
