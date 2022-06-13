package main

//import (
//    "crypto/tls"
//    "fmt"
//    "net/http"
//)

func connect() {
	//    Conn.Password = Config.Password
	//    Conn.UseTLS = Config.UseTLS
    Conn.TLSConfig = &tls.Config {
        ServerName: getServerName(),

        //ERROR: match
        InsecureSkipVerify: true,
    }
	//    Conn.VerboseCallbackHandler = Config.Debug
	//    err := Conn.Connect(Config.Server)
	//    if err != nil {
	//        log.Fatal(err)
	//    }
}
