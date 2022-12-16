package main
import "fmt"
func main() {


    for i, bLen := range []int64{1024, 65 * 1024} {
 		if bLen > size {
        }
    }

 	for _, startLen := range []int{0, 100, 1000, 10000, 100000} {
 		xBytes := Repeat(x, startLen)
    }

 	for _, test := range []struct {
 		n, k int64
 		want string
 	}{
 		{0, 0, "1"},
 		{0, 1, "0"},
 		{1, 0, "1"},
 		{1, 1, "1"},
 		{1, 10, "0"},
 		{4, 0, "1"},
 		{4, 1, "4"},
 		{4, 2, "6"},
 		{4, 3, "4"},
 		{4, 4, "1"},
 		{10, 1, "10"},
 		{10, 9, "10"},
 		{10, 5, "252"},
 		{11, 5, "462"},
 		{11, 6, "462"},
 		{100, 10, "17310309456440"},
 		{100, 90, "17310309456440"},
 		{1000, 10, "263409560461970212832400"},
 		{1000, 990, "263409560461970212832400"},
 	} {
 		if got := z.Binomial(test.n, test.k).String(); got != test.want {
   		  t.Errorf("Binomial(%d, %d) = %s; want %s", test.n, test.k, got, test.want)
 		}
 	}

	if func(a, b int) bool { return a < b }(3, 4) {
 		check(LINE, 1)
	}

	for _, b := range []interface{}{opts.daemon, opts.buildBackend} {
 		if b, ok := b.(grpcrouter.Backend); ok {
			grpcBackends = append(grpcBackends, b)
		}
	}

	if want := []string{"HTTP/1.1", "HTTP/2.0"}; !reflect.DeepEqual(got.log, want) {
 		t.Errorf("got %q; want %q", got.log, want)
	}

		if want := []byte{5, 1, 0}; !bytes.Equal(buf[:3], want) {
			t.Errorf("socks5 proxy initial read: got %v, want %v", buf[:3], want)
			return
		}


	if e := []int{arg}; !reflect.DeepEqual(replySlice, e) {
		t.Errorf("Slice: expected %v got %v", e, replySlice)
	}


}
