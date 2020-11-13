package main

import (
	"golang.org/x/crypto/ssh"
)

func ok() {
	var publicKey *rsa.PublicKey

	privateKey, err := rsa.GenerateKey(rand.Reader, 2048)
	if err != nil {
		return nil, nil, err
	}
	publicKey = &privateKey.PublicKey
	hostKey, _ := ssh.NewPublicKey(publicKey)
	// ok
	_ = ssh.FixedHostKey(hostKey);
}

func main() {
	// ruleid: avoid-ssh-insecure-ignore-host-key
	_ = ssh.InsecureIgnoreHostKey()
}
