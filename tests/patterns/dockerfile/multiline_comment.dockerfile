FROM alpine:3.19

CMD echo "foo" && \
   # Bug happens becuase of this comment (remove comment and see no bug) \
   # MATCH:
   echo "bar"

# MATCH:
CMD echo "bar"