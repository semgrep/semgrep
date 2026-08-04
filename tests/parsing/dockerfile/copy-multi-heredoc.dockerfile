# syntax=docker/dockerfile:1
FROM alpine
COPY <<robots.txt <<humans.txt /usr/share/nginx/html/
User-agent: *
Disallow:
robots.txt
Hi humans
humans.txt
