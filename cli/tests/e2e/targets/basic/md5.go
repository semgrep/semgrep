package main

import (
    "crypto/md5"
)

func main() {
    var str string = "hello world"

    hasher := md5.New()
    hasher.Write([]byte(str))
}
