{rules: [
 { id: "ruby-jwt-hardcoded-secret",
   message: |||
    Hardcoded JWT secret or private key is used.
    This is a Insufficiently Protected Credentials weakness: https://cwe.mitre.org/data/definitions/522.html
    Consider using an appropriate security mechanism to protect the credentials (e.g. keeping secrets in environment variables)
  |||,
  metadata: {
    cwe: 'CWE-522: Insufficiently Protected Credentials',
    owasp: 'A2: Broken Authentication',
    "source-rule-url": 'https://r2c.dev/blog/2020/hardcoded-secrets-unverified-tokens-and-other-common-jwt-mistakes/',
  },
  match: {
   and: [
      |||   
       require 'jwt'
       ...
      |||,
      { or: [
        'JWT.encode($PAYLOAD,"...",...)',
        'JWT.decode($PAYLOAD,"...",...)',
        'JWT.encode($PAYLOAD,nil,...)',
        'JWT.decode($PAYLOAD,nil,...)',
        |||
         $SECRET = "..."
         ...
         JWT.encode($PAYLOAD,$SECRET,...)
        |||,
        |||
         $SECRET = "..."
         ...
         JWT.decode($PAYLOAD,$SECRET,...)
        |||,
      ]
     }
   ]
  },
  languages: ["ruby"],
  severity: "ERROR",
 },
 ]
}

