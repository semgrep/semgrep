# Python 3.15: PEP 798 unpacking comprehensions and generator expressions.
[*items for items in groups]
{*items for items in groups}
{**mapping for mapping in mappings}
(*items for items in groups)
[*("prefix", item) for item in items]
{*("prefix", item) for item in items}
(*("prefix", item) for item in items)
{**make_mapping(key) for key in keys if keep(key)}
[*items for group in groups for items in group if items]

async def flatten() -> None:
    [*items async for items in groups()]
    {*items async for items in groups()}
    {**mapping async for mapping in mappings()}
    (*items async for items in groups())
