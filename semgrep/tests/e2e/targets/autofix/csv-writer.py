import csv

# ruleid:unquoted-csv-writer
csv.writer(csvfile, delimiter=',', quotechar='"')
# ok
csv.writer(csvfile, delimiter=',', quotechar='"', quoting=csv.QUOTE_ALL)