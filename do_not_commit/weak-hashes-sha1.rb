require 'digest'
class bad_md5
    def bad_md5_code() 
        # ruleid: weak-hashes-md5
        sha = Digest::SHA1.hexdigest 'abc' 
        # ruleid: weak-hashes-md5
        sha = Digest::SHA1.new
        # ruleid: weak-hashes-md5
        sha = Digest::SHA1.base64digest 'abc'
        # ruleid: weak-hashes-md5
        sha = Digest::SHA1.digest 'abc'
        
        # ruleid: weak-hashes-md5
        digest = OpenSSL::Digest::SHA1.new
        # ruleid: weak-hashes-md5
        digest = OpenSSL::Digest::SHA1.hexdigest 'abc' 
        # ruleid: weak-hashes-md5
        digest = OpenSSL::Digest::SHA1.new
        # ruleid: weak-hashes-md5
        digest = OpenSSL::Digest::SHA1.base64digest 'abc'
        # ruleid: weak-hashes-md5
        digest = OpenSSL::Digest::SHA1.digest 'abc'
        # ruleid: weak-hashes-md5
        OpenSSL::HMAC.hexdigest("sha1", key, data)
        # ok:
        OpenSSL::HMAC.hexdigest("SHA256", key, data)
        # ok:
        digest = OpenSSL::Digest::SHA256.new
        # ok:
        digest = OpenSSL::Digest::SHA256.hexdigest 'abc' 
    end
end
