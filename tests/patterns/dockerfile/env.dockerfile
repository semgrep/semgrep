# MATCH:
ENV MY_NAME="John Doe"

ENV MY_DOG=Rex\ The\ Dog MY_CAT=fluffy
ENV MY_GOLDFISH Maurice

# MATCH:
ENV MY_NAME "John Doe"

ENV MY_GOLDFISH=Maurice MY_NAME="John Doe"
