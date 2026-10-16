# ERROR: match
x = "%{ if enabled }${danger(input)}%{ endif }"
y = "%{ if enabled }${safe(input)}%{ endif }"
z = "%{ if enabled }${danger(input)}%{ else }safe%{ endif }"
