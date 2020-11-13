# cf. https://makandracards.com/makandra/2579-everything-you-know-about-html_safe-is-wrong

# ok: avoid-html-safe
"foo".length

# ruleid: avoid-html-safe
"foo".html_safe

# ruleid: avoid-html-safe
"<div>foo</div>".html_safe + "<bar>"

# ruleid: avoid-html-safe
html = "<div>".html_safe

# ok: avoid-html-safe
html = "<div>"

# ruleid: avoid-html-safe
"<div>".html_safe.tap
