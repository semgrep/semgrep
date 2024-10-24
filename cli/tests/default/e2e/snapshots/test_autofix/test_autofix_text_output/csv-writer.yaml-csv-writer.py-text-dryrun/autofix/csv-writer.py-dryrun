import csv

csvfile="/tmp/blah.csv"

def get_file():
    return csvfile

# ruleid:unquoted-csv-writer
csv.writer(csvfile, delimiter=',', quotechar='"')

# ruleid:unquoted-csv-writer
csv.writer(get_file(), delimiter=',', quotechar='"')

# ok
csv.writer(csvfile, delimiter=',', quotechar='"', quoting=csv.QUOTE_ALL)