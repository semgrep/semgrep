package main

import (
	"archive/zip"
	"io"
	"os"
	"strconv"
)

func main() {
	// ruleid: potential-dos-via-decompression-bomb
	r, err := zip.OpenReader("tmp.zip")
	if err != nil {
		panic(err)
	}
	defer r.Close()

	for i, f := range r.File {
		out, err := os.OpenFile("output"+strconv.Itoa(i), os.O_WRONLY|os.O_CREATE|os.O_TRUNC, f.Mode())
		if err != nil {
			panic(err)
		}

		rc, err := f.Open()
		if err != nil {
			panic(err)
		}

		_, err = io.Copy(out, rc)

		out.Close()
		rc.Close()

		if err != nil {
			panic(err)
		}
	}
}
