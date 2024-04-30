FROM library/redis:6.2.14
ARG EXPIRES
LABEL quay.expires-after=$EXPIRES