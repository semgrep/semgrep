# from https://github.com/returntocorp/semgrep/issues/912

# note that semgrep does not report this one, because it
# requires a pattern-inside instead of just pattern
# but semgrep-core reports it :)
#ruleid: boto3-internal-network
boto3.client(host="192.168.1.125")

# Okay
boto3.client(host="https://bucket.s3.amazonaws.com")
