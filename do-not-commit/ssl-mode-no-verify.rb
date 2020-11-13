# cf. https://github.com/presidentbeef/brakeman/blob/df2ac8c98a649a5f7b47a42bc17d2ce4ab0e26ec/docs/warning_types/ssl_verification_bypass/index.markdown

require "net/https"
require "uri"

uri = URI.parse("https://ssl-site.com/")
http = Net::HTTP.new(uri.host, uri.port)
http.use_ssl = true
# ruleid:ssl-mode-no-verify
http.verify_mode = OpenSSL::SSL::VERIFY_NONE

request = Net::HTTP::Get.new(uri.request_uri)

http.verify_mode = OpenSSL::SSL::VERIFY_PEER

response = http.request(request)

# ok
http.verify_mode = OpenSSL::SSL::VERIFY_PEER
request = Net::HTTP::Get.new(uri.request_uri)
response = http.request(request)
