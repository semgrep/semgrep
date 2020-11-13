package insecuregrpc

import (
    "crypto/x509"
    "net/http"
    "net/http/httptest"

    "google.golang.org/grpc"
    "google.golang.org/grpc/credentials"
)

// cf. https://blog.gopheracademy.com/advent-2019/go-grps-and-tls/#connection-without-encryption
func unsafe() {
    // Server
    // ruleid:grpc-server-insecure-connection
    s := grpc.NewServer()
    // ... register gRPC services ...
    if err = s.Serve(lis); err != nil {
        log.Fatalf("failed to serve: %v", err)
    }
}

func safe() {
    // Server
    // ok
    s := grpc.NewServer(grpc.Creds(credentials.NewClientTLSFromCert(x509.NewCertPool(), "")))
    // ... register gRPC services ...
    if err = s.Serve(lis); err != nil {
        log.Fatalf("failed to serve: %v", err)
    }
}

// False Positive test
// cf. https://github.com/daghan/invoicer-chapter2/blob/4c5b00408a4aeece86d98ad3ef1c88e610053dfc/vendor/golang.org/x/net/websocket/websocket_test.go#L129
func startServer() {
	http.Handle("/echo", Handler(echoServer))
	http.Handle("/count", Handler(countServer))
	http.Handle("/ctrldata", Handler(ctrlAndDataServer))
	subproto := Server{
		Handshake: subProtocolHandshake,
		Handler:   Handler(subProtoServer),
	}
	http.Handle("/subproto", subproto)
    // ok
	server := httptest.NewServer(nil)
	serverAddr = server.Listener.Addr().String()
	log.Print("Test WebSocket server listening on ", serverAddr)
}