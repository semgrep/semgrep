module github.com/foo

go 1.23

toolchain go1.23.4

godebug (
	default=go1.21
	panicnil=1
)

godebug asynctimerchan=1

require (
	github.com/go-chi/chi/v5 v5.0.7
	golang.org/x/sys v0.2.0 // indirect
)
