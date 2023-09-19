def foo
    #
    # Test bad readlines combinations

    # ruleid: avoid-tainted-file-access
    File.readlines("/tmp/#{params[:name]}")
    # ok: avoid-tainted-file-access
    File.readlines("/tmp/usr/bin")

    # ruleid: avoid-tainted-file-access
    Dir.readlines("/tmp/#{params[:name]}")
    # ok: avoid-tainted-file-access
    Dir.readlines("/tmp/usr/bin")

    # ruleid: avoid-tainted-file-access
    IO.readlines("/tmp/#{params[:name]}")
    # ok: avoid-tainted-file-access
    IO.readlines("/tmp/usr/bin")

    # ruleid: avoid-tainted-file-access
    Kernel.readlines("/tmp/#{params[:name]}")
    # ok: avoid-tainted-file-access
    Kernel.readlines("/tmp/usr/bin")

    # ruleid: avoid-tainted-file-access
    PStore.readlines("/tmp/#{cookies[:name]}")
    # ok: avoid-tainted-file-access
    PStore.readlines("/tmp/#{anything}/bin")

    # ruleid: avoid-tainted-file-access
    Pathname.readlines("/tmp/#{request.env[:name]}")
    # ok: avoid-tainted-file-access
    Pathname.readlines("/tmp/#{anything}/bin")

end
