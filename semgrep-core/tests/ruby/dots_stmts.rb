def foo(user)
    # ERROR:
    user_data = get_data user
    puts "... more stuff here ..."
    eval user_data

end
