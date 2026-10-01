# ERROR: match
f(r"---(hello)---")
# ERROR: match
f(r"(hello)")
# ERROR: match
f(R"[hello]")
# ERROR: match
f(r"{hello}")
f(r"(world)")
