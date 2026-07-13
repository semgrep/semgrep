# syntax=docker/dockerfile:1
FROM alpine
RUN <<-EOF
	apt-get update
	apt-get install -y curl
EOF
