FROM busybox

# ruleid: use-absolute-workdir
WORKDIR usr/src/app

# ok: use-absolute-workdir
WORKDIR /usr/src/app

ENV dirpath1=bar
# ruleid: use-absolute-workdir
WORKDIR ${dirpath1}

ENV dirpath2=/bar
# ok: use-absolute-workdir
WORKDIR ${dirpath2}