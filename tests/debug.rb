def foo
    #
    # Test bad readlines combinations

    # ruleid: avoid-tainted-file-access
    File.readlines("/tmp/#{params[:name]}")
    # ok: avoid-tainted-file-access
    File.readlines("/tmp/usr/bin")

end
