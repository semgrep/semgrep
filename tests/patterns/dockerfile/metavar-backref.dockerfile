# MATCH:
ENV PORT 80
RUN echo hello
RUN echo 80

ENV PORT 80
RUN echo hello
RUN echo 100
