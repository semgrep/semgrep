# ruleid: unpack-list
[*items for items in groups]
# ruleid: unpack-set
{*items for items in groups}
# ruleid: unpack-dict
{**mapping for mapping in mappings}
# ruleid: unpack-generator
(*items for items in groups)

# ok: unpack-list
[items for items in groups]
# ok: unpack-set, unpack-dict
{items for items in groups}
# ok: unpack-generator
(items for items in groups)
