from framework import db_access


def fn(params):
    params["sql"] = "select value from table where x = %s" % params["test"]
    # ruleid: sql-injection
    db_access.mysql_dict(params)


def fn(params):
    params["sql"] = "select value from table where x = %s" % db_access.escape(
        params["test"]
    )
    # ok: sql-injection
    db_access.mysql_dict(params)


def fn(params):
    params["sql"] = "select xyz from table"
    # ok: sql-injection
    results = db_access.mysql_dict(params)

    # Just testing source/sink match here, accurate code is below in the for loop
    params["sql"] = "delete from table2 where field = %s" % results
    # ruleid: sql-injection
    db_access.mysql_update(params)

    for res in results:
        params["sql"] = "delete from table2 where field = %s" % res["xyz"]
        # ruleid: sql-injection
        db_access.mysql_update(params)


def fn(params):
    params["name"] = "test"
    params["sql"] = "select * from params where name = %(name)s" % params
    # ok: sql-injection
    db_access.mysql_update(params)


def fn(params):
    alt = params

    params["sql"] = "select * from params where name = %(name)s" % alt
    # ruleid: sql-injection
    db_access.mysql_update(params)


def fn(params):
    alt = params
    params['name'] = 'x'
    params["sql"] = "select * from params where name = %(name)s" % alt
    # todook: sql-injection
    db_access.mysql_update(params)


def fn(params):
    alt = params.copy()
    params['name'] = 'x'

    params["sql"] = "select * from params where name = %(name)s" % alt
    # ruleid: sql-injection
    db_access.mysql_update(params)
