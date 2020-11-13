# cf. https://github.com/we45/Vulnerable-Flask-App/blob/752ee16087c0bfb79073f68802d907569a1f0df7/app/app.py#L96

import jwt
from jwt.exceptions import DecodeError, MissingRequiredClaimError, InvalidKeyError

def verify_jwt(token):
    try:
        # ok
        decoded = jwt.decode(token, app.config['SECRET_KEY_HMAC'], verify=True, issuer = 'we45', leeway=10, algorithms=['HS256'])
        print("JWT Token from API: {0}".format(decoded))
        return True
    except DecodeError:
        print("Error in decoding token")
        return False
    except MissingRequiredClaimError as e:
        print('Claim required is missing: {0}'.format(e))
        return False

def insecure_verify(token):
    # ruleid:unverified-jwt-decode
    decoded = jwt.decode(token, verify = False)
    print(decoded)
    return True
