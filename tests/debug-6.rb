def foo

    #
    # Test bad open combinations

    # ruleid: avoid-tainted-file-access
    File.open("/tmp/#{params[:name]}")
    # ok: avoid-tainted-file-access
    File.open("/tmp/usr/bin")

    # ruleid: avoid-tainted-file-access
    Dir.open("/tmp/#{params[:name]}")
    # ok: avoid-tainted-file-access
    Dir.open("/tmp/usr/bin")

    # ruleid: avoid-tainted-file-access
    IO.open("/tmp/#{params[:name]}")
    # ok: avoid-tainted-file-access
    IO.open("/tmp/usr/bin")

    # ruleid: avoid-tainted-file-access
    Kernel.open("/tmp/#{params[:name]}")
    # ok: avoid-tainted-file-access
    Kernel.open("/tmp/usr/bin")

    # ruleid: avoid-tainted-file-access
    PStore.open("/tmp/#{params[:name]}")
    # ok: avoid-tainted-file-access
    PStore.open("/tmp/usr/bin")

    # ruleid: avoid-tainted-file-access
    Pathname.open("/tmp/#{params[:name]}")
    # ok: avoid-tainted-file-access
    Pathname.open("/tmp/usr/bin")

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


    #
    # Test ok tainted calls

    # ok: avoid-tainted-file-access
    File.basename("/tmp/#{params[:name]}")

end
