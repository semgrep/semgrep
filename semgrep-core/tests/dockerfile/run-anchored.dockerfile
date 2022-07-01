# TODO: we would like to forbid the first match below, but doing it by
# default introduces too many regressions because people like
# to write 'RUN apt-get ...'  and that it matches also 'RUN apt-get stuff && stuff'
# or even 'RUN sudo apt-get ...'
# use options: implicit_deep_exprstmt: false to change the behavior and force
# the "anchor"

# MATCH:
RUN a && b

# MATCH:
RUN b
