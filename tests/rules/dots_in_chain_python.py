# ruleid: dots-in-chain
a.b.c.d

# ruleid: dots-in-chain
a.x.d

# ok: dots-in-chain
a.b.c.e

# ruleid: dots-in-call-chain
result = (
    builder()
    .set_subject("Bob")
    .sign_with(key)
    .compact()
)

# ruleid: dots-in-call-chain
builder().compact()

# ok: dots-in-call-chain
builder().finish()
