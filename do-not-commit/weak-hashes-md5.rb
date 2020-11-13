require 'digest'
class bad_md5
    def bad_md5_code()
        # ruleid: weak-hashes-md5
        md5 = Digest::MD5.hexdigest 'abc'
        # ruleid: weak-hashes-md5
        md5 = Digest::MD5.new
        # ruleid: weak-hashes-md5
        md5 = Digest::MD5.base64digest 'abc'
        # ruleid: weak-hashes-md5
        md5 = Digest::MD5.digest 'abc'

        # ruleid: weak-hashes-md5
        digest = OpenSSL::Digest::MD5.new
        # ruleid: weak-hashes-md5
        digest = OpenSSL::Digest::MD5.hexdigest 'abc'
        # ruleid: weak-hashes-md5
        digest = OpenSSL::Digest::MD5.new
        # ruleid: weak-hashes-md5
        digest = OpenSSL::Digest::MD5.base64digest 'abc'
        # ruleid: weak-hashes-md5
        digest = OpenSSL::Digest::MD5.digest 'abc'
    end
end
