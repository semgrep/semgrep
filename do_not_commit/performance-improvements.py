# ruleid:batch-import
for song in songs:
    db.session.add(song)

# ruleid:len-all-count
len(persons.all())
