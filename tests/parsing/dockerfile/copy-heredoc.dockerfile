# syntax=docker/dockerfile:1
FROM nginx
COPY <<EOF /usr/share/nginx/html/index.html
<html><body>Hello</body></html>
EOF
