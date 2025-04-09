#ok: date_rule
day1 = "3000-01-15"
#ruleid: date_rule
day2 = "1970-01-01"  # TODO: set this date to 1906-12-09 after fixing
                     # date parsing on Windows. On Windows this leads
                     # to `exception: Unix_error: Result too large mktime`
                     # because the date is before the epoch. This is a
                     # version of what is discussed in
                     # https://github.com/neo4j/neo4j-python-driver/issues/302
#ok: date_rule
day3 = "this is not a date"
