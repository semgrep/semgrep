# TODO: we would like to forbid the first match below, but doing it by
# default introduces too many regressions because people like
# to write 'RUN apt-get ...'  and that it matches also 'RUN apt-get stuff && stuff'

# MATCH:
{ a && b; }

# MATCH:
{ a; }
