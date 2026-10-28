// ERROR: match
for await value in values() { use(value) }
for value in await values() { use(value) }
for value in values() { use(value) }
