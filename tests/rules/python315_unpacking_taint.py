def list_unpacking() -> None:
    result = [*items for items in source()]
    # ruleid: unpacking-taint
    sink(result)


def set_unpacking() -> None:
    result = {*items for items in source()}
    # ruleid: unpacking-taint
    sink(result)


def dict_unpacking() -> None:
    result = {**mapping for mapping in source()}
    # ruleid: unpacking-taint
    sink(result)


def generator_unpacking() -> None:
    result = (*items for items in source())
    # ruleid: unpacking-taint
    sink(result)


def clean_unpacking() -> None:
    result = [*items for items in [[1, 2], [3]]]
    # ok: unpacking-taint
    sink(result)


def ordinary_comprehension_body() -> None:
    result = [source() for item in [1, 2]]
    # ruleid: unpacking-taint
    sink(result)


def dictionary_literal_spread() -> None:
    result = {**source()}
    # ruleid: unpacking-taint
    sink(result)


def dictionary_comprehension_body() -> None:
    result = {key: source() for key in ["item"]}
    # ruleid: unpacking-taint
    sink(result)


def clean_dictionary_comprehensions() -> None:
    mappings = [{"item": 1}]
    unpacked = {**mapping for mapping in mappings}
    ordinary = {key: 1 for key in ["item"]}
    # ok: unpacking-taint
    sink(unpacked)
    # ok: unpacking-taint
    sink(ordinary)


def dictionary_spread_preserves_earlier_taint() -> None:
    result = {**source(), **{}}
    # ruleid: unpacking-taint
    sink(result)
