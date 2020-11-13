def test_bad_is_1():
    # ruleid:bad-operator-in-filter
    Model.query.filter(Model.id is 5).first()

def test_bad_and_1():
    # ruleid:bad-operator-in-filter
    Model.query.filter(Model.id == 5 and Model.name == 'hi').first()

def test_bad_or_1():
    # ruleid:bad-operator-in-filter
    Model.query.filter(Model.id == 5 or Model.name == 'hi').first()

def test_bad_in_1():
    # ruleid:bad-operator-in-filter
    Model.query.filter(Model.id in [1, 2, 3]).first()

def test_bad_not_1():
    # ruleid:bad-operator-in-filter
    Model.query.filter(not Model.id == 5).first()

def test_bad_not_2():
    # ruleid:bad-operator-in-filter
    Model.query.filter(Model.id is not 5).first()

def test_bad_not_3():
    # ruleid:bad-operator-in-filter
    Model.query.filter(Model.id == 5 and not Model.name == 'hi').first()

def test_bad_not_4():
    # ruleid:bad-operator-in-filter
    Model.query.filter(Model.id == 5 or not Model.name == 'hi').first()

def test_bad_not_5():
    # ruleid:bad-operator-in-filter
    Model.query.filter(Model.id not in [1, 2, 3]).first()

def test_ok_1():
    model = Model.query.first()
    # ok
    return model.id is 5

