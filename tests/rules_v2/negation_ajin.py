import os
#ruleid: rule_template_id
x = os.environ
#ok:
verify = (os.environ.get('REQUESTS_CA_BUNDLE') or os.environ.get('CURL_CA_BUNDLE'))
#ok:
os.environ[env_name] = 1
k = None
#ok:
get_proxy = os.environ.get(k) or os.environ.get(k.upper())
