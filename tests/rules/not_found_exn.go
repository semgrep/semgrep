package dkeyczar

func generateRSAKey(size uint) (*rsaKey, error) {

	return rsa.GenerateKey(rand.Reader, int(size))

}
