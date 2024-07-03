def test_function():
    id = request
    try:
        id = int(id)
    except Exception:
        raise BadRequest()

    some_object = id

    csv_file = get_csv(some_object)

    # ok: test
    return send_file(
        csv_file
    )
