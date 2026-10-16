# ERROR: match
x = "%{ for v in items }${danger(v)}%{ endfor }"
y = "%{ for v in items }${safe(v)}%{ endfor }"
