def bad_file_disclosure
    # ruleid: file-disclosure
    config.serve_static_assets = true
end

def ok_file_disclosure
    # ok
    config.serve_static_assets = false
end
