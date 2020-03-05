package Foo

//ERROR:
import _ "foo/bar"
//ERROR:
import . "foo/bar"
//ERROR:
import b "foo/bar"
//ERROR:
import "foo/bar"

//ERROR:
import (
	"foo/bar"
	"net/http"
)

//ERROR:
import (
. "foo/bar"
	_ "foo/bar"
)
