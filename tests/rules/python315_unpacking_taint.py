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
