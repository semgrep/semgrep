# ruleid: lazy-only, import-json
lazy import json
# ruleid: resolved-json-call
json.loads(data)

# ruleid: lazy-only
lazy import json as decoder
# ruleid: resolved-json-call
decoder.loads(data)

# ruleid: lazy-from-only
lazy from json import loads as decode
# ruleid: resolved-json-call
decode(data)

# ok: lazy-only
# ruleid: import-json
import json

# ok: lazy-only
lazy = 1

# ok: lazy-from-only
# ruleid: import-json
from json import loads
