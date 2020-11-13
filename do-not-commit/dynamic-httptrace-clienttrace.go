package uhoh

import (
	"context"
	"net"
	"net/http"
	"net/http/httptrace"
)

func WithTrace(req *http.Request, trace *httptrace.ClientTrace) *http.Request {
    // ruleid: dynamic-httptrace-clienttrace
	return req.WithContext(httptrace.WithClientTrace(req.Context(), trace))
}
