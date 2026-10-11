package go124

type Box[T any] struct{ Value T }

type ParenthesizedNamed interface { (int) | ~[]byte }
type ParenthesizedApproxNamed interface { ~(int) | string }
type ParenthesizedGeneric interface { (Box[int]) | ~[]byte }
type ParenthesizedPointerOperand interface { *(Box[int]) | ~[]byte }
type ParenthesizedApproxSlice interface { ~([]byte) | ~map[string]bool }
type NestedParenthesizedSlice interface { ~(([]byte)) | ~int }
type LiteralInterfaceUnion interface { interface{} | ~[]byte }
type ConstrainedInterfaceUnion interface { interface{ ~int } | ~[]byte }
type ParenthesizedApproxPointer interface { ~(*int) | ~[]byte }

func scanAfterParenthesizedInterface(xs []int) {
    for range xs {
        println("still observed")
    }
}
