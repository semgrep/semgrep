# ERROR: match
x = "%{ if enabled }${danger(input)}%{ else }safe%{ endif }"
y = "%{ if enabled }${safe(input)}%{ else }safe%{ endif }"
z = "%{ if enabled }safe%{ else }${danger(input)}%{ endif }"
