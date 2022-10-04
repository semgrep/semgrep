FROM debian

# MATCH:
CMD echo hello

# MATCH:
CMD ls \
  | tac

# MATCH:
CMD ls \
  -l

# MATCH:
CMD ls \
  -l; # blah \
  echo yay

# MATCH:
CMD ["ls"]
