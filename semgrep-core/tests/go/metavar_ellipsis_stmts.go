// +build gofuzz

package sequitur

import "bytes"

//ERROR: match
func Fuzz(data []byte) int {
        if len(data) == 0 {
                return 0
        }

        var b bytes.Buffer

        g := Parse(data)
        g.Print(&b)

        if !bytes.Equal(b.Bytes(), data) {
                panic("roundtrip mismatch")
        }

        return 0
}

