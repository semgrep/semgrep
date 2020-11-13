 def bad_ssl
    # ruleid: force-ssl-false
    config.force_ssl = false
 end

 def ok_ssl
    # ok
    config.force_ssl = true
 end
