FROM alpine:3.19

# MATCH:
CMD echo "bar"

CMD echo "foo" && \
   # Bug happens because of this comment (remove comment and see no bug) \
   # MATCH:
   echo "bar"
