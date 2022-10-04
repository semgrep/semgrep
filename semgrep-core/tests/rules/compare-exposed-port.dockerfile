# cf. https://github.com/hadolint/hadolint/wiki/DL3011

FROM busybox

# ok: invalid-port
EXPOSE 65535

# ruleid: invalid-port
EXPOSE 65536

# ok: invalid-port
EXPOSE 0
