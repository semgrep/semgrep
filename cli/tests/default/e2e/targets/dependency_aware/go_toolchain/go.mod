// comment
// second comment
module github.com/foo

go 1.18

toolchain go1.21.0

retract (
    v1.0.0
    [v1.0.0, v1.9.9]
)

require (
	github.com/go-chi/chi/v5 v5.0.7
	github.com/go-chi/render v1.0.2
	github.com/lucasb-eyer/go-colorful v1.2.0
	go.opentelemetry.io/otel/sdk v1.11.1
	github.com/cheekybits/genny v99.99.99
)

require (
	github.com/ajg/form v1.5.1 // indirect
	github.com/cenkalti/backoff/v4 v4.1.3 // indirect
	github.com/felixge/httpsnoop v1.0.2 // indirect
	github.com/go-logr/logr v1.2.3 // indirect
	github.com/go-logr/stdr v1.2.2 // indirect
	github.com/golang/protobuf v1.5.2 // indirect
	github.com/grpc-ecosystem/grpc-gateway/v2 v2.7.0 // indirect
	go.opentelemetry.io/contrib v1.0.0 // indirect
	go.opentelemetry.io/otel/exporters/otlp/internal/retry v1.11.1 // indirect
	go.opentelemetry.io/otel/trace v1.11.1 // indirect
	go.opentelemetry.io/proto/otlp v0.19.0 // indirect
	golang.org/x/net v0.0.0-20220722155237-a158d28d115b // indirect
	golang.org/x/text v0.3.8 // indirect
	google.golang.org/genproto v0.0.0-20211118181313-81c1377c94b1 // indirect
	google.golang.org/grpc v1.50.1 // indirect
)

require (
	// comment
	// second comment
	github.com/riandyrn/otelchi v0.5.0
	go.opentelemetry.io/otel v1.11.1
	go.opentelemetry.io/otel/exporters/otlp/otlptrace v1.11.1
	go.opentelemetry.io/otel/exporters/otlp/otlptrace/otlptracehttp v1.11.1
	golang.org/x/sys v0.2.0 // indirect
	// interspersed comment
	golang.org/x/tools v0.1.12
	google.golang.org/protobuf v1.28.1 // indirect
)

replace (
		// my comment with ending ()
    github.com/riandyrn/otelchi v0.5.0 => github.com/riandyrn/otelchi v0.5.1
   	go.opentelemetry.io/otel v1.11.1 => go.opentelemetry.io/otel v1.11.2
)