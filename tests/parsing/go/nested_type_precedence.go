package precedence

var send chan<- chan int
var receive <-chan chan<- int
var functions func() chan<- []map[string]func() int
var array [2]func() []int
var pointer *func() *int
