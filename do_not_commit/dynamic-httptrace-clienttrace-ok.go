/*
 * Test case reference:
 * cf. https://github.com/containous/traefik//blob/bb4de11c517dfa4a6f6ca446732f4b55f771cb49/pkg/middlewares/retry/retry.go
 */

package retry

import (
	"bufio"
	"context"
	"fmt"
	"io/ioutil"
	"net"
	"net/http"
	"net/http/httptrace"
    "time"

	"github.com/containous/traefik/v2/pkg/config/dynamic"
	"github.com/containous/traefik/v2/pkg/log"
	"github.com/containous/traefik/v2/pkg/middlewares"
	"github.com/containous/traefik/v2/pkg/tracing"
	"github.com/opentracing/opentracing-go/ext"
)

// Compile time validation that the response writer implements http interfaces correctly.
var _ middlewares.Stateful = &responseWriterWithCloseNotify{}

const (
	typeName = "Retry"
)

// Listener is used to inform about retry attempts.
type Listener interface {
	// Retried will be called when a retry happens, with the request attempt passed to it.
	// For the first retry this will be attempt 2.
	Retried(req *http.Request, attempt int)
}

// Listeners is a convenience type to construct a list of Listener and notify
// each of them about a retry attempt.
type Listeners []Listener

// retry is a middleware that retries requests.
type retry struct {
	attempts int
	next     http.Handler
	listener Listener
	name     string
}

// New returns a new retry middleware.
func New(ctx context.Context, next http.Handler, config dynamic.Retry, listener Listener, name string) (http.Handler, error) {
	log.FromContext(middlewares.GetLoggerCtx(ctx, name, typeName)).Debug("Creating middleware")

	if config.Attempts <= 0 {
		return nil, fmt.Errorf("incorrect (or empty) value for attempt (%d)", config.Attempts)
	}

	return &retry{
		attempts: config.Attempts,
		next:     next,
		listener: listener,
		name:     name,
	}, nil
}

func (r *retry) GetTracingInformation() (string, ext.SpanKindEnum) {
	return r.name, tracing.SpanKindNoneEnum
}

func (r *retry) ServeHTTP(rw http.ResponseWriter, req *http.Request) {
	// if we might make multiple attempts, swap the body for an ioutil.NopCloser
	// cf https://github.com/containous/traefik/issues/1008
	if r.attempts > 1 {
		body := req.Body
		defer body.Close()
		req.Body = ioutil.NopCloser(body)
	}

	attempts := 1
	for {
		shouldRetry := attempts < r.attempts
		retryResponseWriter := newResponseWriter(rw, shouldRetry)

		// Disable retries when the backend already received request data
		trace := &httptrace.ClientTrace{
			WroteHeaders: func() {
				retryResponseWriter.DisableRetries()
			},
			WroteRequest: func(httptrace.WroteRequestInfo) {
				retryResponseWriter.DisableRetries()
			},
		}
        // ok
		newCtx := httptrace.WithClientTrace(req.Context(), trace)

		r.next.ServeHTTP(retryResponseWriter, req.WithContext(newCtx))

		if !retryResponseWriter.ShouldRetry() {
			break
		}

		attempts++

		log.FromContext(middlewares.GetLoggerCtx(req.Context(), r.name, typeName)).
			Debugf("New attempt %d for request: %v", attempts, req.URL)

		r.listener.Retried(req, attempts)
	}
}

// Retried exists to implement the Listener interface. It calls Retried on each of its slice entries.
func (l Listeners) Retried(req *http.Request, attempt int) {
	for _, listener := range l {
		listener.Retried(req, attempt)
	}
}

type responseWriter interface {
	http.ResponseWriter
	http.Flusher
	ShouldRetry() bool
	DisableRetries()
}

func newResponseWriter(rw http.ResponseWriter, shouldRetry bool) responseWriter {
	responseWriter := &responseWriterWithoutCloseNotify{
		responseWriter: rw,
		headers:        make(http.Header),
		shouldRetry:    shouldRetry,
	}
	if _, ok := rw.(http.CloseNotifier); ok {
		return &responseWriterWithCloseNotify{
			responseWriterWithoutCloseNotify: responseWriter,
		}
	}
	return responseWriter
}

type responseWriterWithoutCloseNotify struct {
	responseWriter http.ResponseWriter
	headers        http.Header
	shouldRetry    bool
	written        bool
}

func (r *responseWriterWithoutCloseNotify) ShouldRetry() bool {
	return r.shouldRetry
}

func (r *responseWriterWithoutCloseNotify) DisableRetries() {
	r.shouldRetry = false
}

func (r *responseWriterWithoutCloseNotify) Header() http.Header {
	if r.written {
		return r.responseWriter.Header()
	}
	return r.headers
}

func (r *responseWriterWithoutCloseNotify) Write(buf []byte) (int, error) {
	if r.ShouldRetry() {
		return len(buf), nil
	}
	return r.responseWriter.Write(buf)
}

func (r *responseWriterWithoutCloseNotify) WriteHeader(code int) {
	if r.ShouldRetry() && code == http.StatusServiceUnavailable {
		// We get a 503 HTTP Status Code when there is no backend server in the pool
		// to which the request could be sent.  Also, note that r.ShouldRetry()
		// will never return true in case there was a connection established to
		// the backend server and so we can be sure that the 503 was produced
		// inside Traefik already and we don't have to retry in this cases.
		r.DisableRetries()
	}

	if r.ShouldRetry() {
		return
	}

	// In that case retry case is set to false which means we at least managed
	// to write headers to the backend : we are not going to perform any further retry.
	// So it is now safe to alter current response headers with headers collected during
	// the latest try before writing headers to client.
	headers := r.responseWriter.Header()
	for header, value := range r.headers {
		headers[header] = value
	}

	r.responseWriter.WriteHeader(code)
	r.written = true
}

func (r *responseWriterWithoutCloseNotify) Hijack() (net.Conn, *bufio.ReadWriter, error) {
	hijacker, ok := r.responseWriter.(http.Hijacker)
	if !ok {
		return nil, nil, fmt.Errorf("%T is not a http.Hijacker", r.responseWriter)
	}
	return hijacker.Hijack()
}

func (r *responseWriterWithoutCloseNotify) Flush() {
	if flusher, ok := r.responseWriter.(http.Flusher); ok {
		flusher.Flush()
	}
}

type responseWriterWithCloseNotify struct {
	*responseWriterWithoutCloseNotify
}

func (r *responseWriterWithCloseNotify) CloseNotify() <-chan bool {
	return r.responseWriter.(http.CloseNotifier).CloseNotify()
}


/*
 * Test case reference
 * cf. https://github.com/gocolly/colly/blob/b1a8ed2f18144f4b70abcfc18a5e58c68a062389/http_trace.go
 */

 // HTTPTrace provides a datastructure for storing an http trace.
type HTTPTrace struct {
	start, connect    time.Time
	ConnectDuration   time.Duration
	FirstByteDuration time.Duration
}

// trace returns a httptrace.ClientTrace object to be used with an http
// request via httptrace.WithClientTrace() that fills in the HttpTrace.
func (ht *HTTPTrace) trace() *httptrace.ClientTrace {
	trace := &httptrace.ClientTrace{
		ConnectStart: func(network, addr string) { ht.connect = time.Now() },
		ConnectDone: func(network, addr string, err error) {
			ht.ConnectDuration = time.Since(ht.connect)
		},

		GetConn: func(hostPort string) { ht.start = time.Now() },
		GotFirstResponseByte: func() {
			ht.FirstByteDuration = time.Since(ht.start)
		},
	}
	return trace
}

// WithTrace returns the given HTTP Request with this HTTPTrace added to its
// context.
func (ht *HTTPTrace) WithTrace(req *http.Request) *http.Request {
    // ok
	return req.WithContext(httptrace.WithClientTrace(req.Context(), ht.trace()))
}


/*
 * Test case reference
 * cf. https://github.com/mehrdadrad/mylg//blob/616fd5309bb143d3f52ef866b2ffe12135f0dd4e/http/ping/ping.go
 */

 // Ping tries to ping a web server through http
func (p *Ping) Ping() (Result, error) {
	var (
		r     Result
		sTime time.Time
		resp  *http.Response
		req   *http.Request
		err   error
	)

	client := &http.Client{
		CheckRedirect: func(req *http.Request, via []*http.Request) error {
			// Don't follow redirects
			return http.ErrUseLastResponse
		},
		Timeout:   p.timeout,
		Transport: p.transport,
	}

	sTime = time.Now()

	if p.method == "POST" {
		r.Size = len(p.buf)
		reader := strings.NewReader(p.buf)
		req, err = http.NewRequest(p.method, p.url, reader)
	} else {
		req, err = http.NewRequest(p.method, p.url, nil)
	}

	if err != nil {
		return r, err
	}

	// customized header
	req.Header.Add("User-Agent", p.uAgent)
	// context, tracert
	if p.tracerEnabled && !p.quiet {
        // ok
		req = req.WithContext(httptrace.WithClientTrace(req.Context(), tracer(&r)))
	}
	resp, err = client.Do(req)

	if err != nil {
		return r, err
	}
	defer resp.Body.Close()

	r.TotalTime = time.Since(sTime).Seconds()

	if p.method == "GET" {
		body, err := ioutil.ReadAll(resp.Body)
		if err != nil {
			return r, err
		}
		r.Size = len(body)
	} else {
		io.Copy(ioutil.Discard, resp.Body)
	}

	r.StatusCode = resp.StatusCode
	r.Proto = resp.Proto
	return r, nil
}

func tracer(r *Result) *httptrace.ClientTrace {
	var (
		begin   = time.Now()
		elapsed time.Duration
	)

	return &httptrace.ClientTrace{
		ConnectDone: func(network, addr string, err error) {
			elapsed = time.Since(begin)
			begin = time.Now()
			r.Trace.ConnectionTime = elapsed.Seconds() * 1e3
		},
		GotFirstResponseByte: func() {
			elapsed = time.Since(begin)
			begin = time.Now()
			r.Trace.TimeToFirstByte = elapsed.Seconds() * 1e3
		},
	}
}
