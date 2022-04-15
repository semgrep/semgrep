# simple strings
RUN foo bar
RUN foo "bar"
RUN foo 'bar'

# string made of multiple fragments
# ERROR:
RUN foo "${HOME}/bar"
