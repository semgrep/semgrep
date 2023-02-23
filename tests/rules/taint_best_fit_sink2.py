
# ok: test
sink(sanitizer(source) if isset(source) else '')

# ok: test
sink(sanitizer(source) if isset(safe) else '')

# ok: test
sink(sanitizer(safe) if isset(source) else '')

# ok: test
sink(safe if isset(source) else '')

