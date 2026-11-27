// Same sites as csharp_preproc_branches.cs, with the finding in the `#if`
// branch: the first branch must be kept as well as the others.
#if DEBUG
class TopIf {
    void M() {
        // ruleid: sink-source
        sink(source());
    }
}
#else
class TopElse { }
#endif

class C {
    void Body() {
#if DEBUG
        // ruleid: sink-source
        sink(source());
#else
        f();
#endif
    }

#if DEBUG
    void MemberIf() {
        // ruleid: sink-source
        sink(source());
    }
#else
    void MemberElse() { }
#endif

    void Expr() {
        var x = 1 +
#if A
            // ruleid: sink-source
            sink(source())
#else
            f(2)
#endif
            ;
    }

    void Initializer() {
        var xs = new List<object> {
#if A
            // ruleid: sink-source
            sink(source()),
#else
            f(1),
#endif
        };
    }

    void Collection() {
        int[] xs = [
#if A
            // ruleid: sink-source
            sink(source()),
#else
            f(1),
#endif
        ];
    }

    object ReturnTail(bool c) {
        return c ? f(1) :
#if A
            // ruleid: sink-source
            sink(source());
#else
            f(2);
#endif
    }

    void ElseStmt(bool a) {
        if (a) { }
#if A
        else {
            // ruleid: sink-source
            sink(source());
        }
#else
        else { f(1); }
#endif
    }

    void FuncBody()
#if A
    {
        // ruleid: sink-source
        sink(source());
    }
#else
    { f(1); }
#endif
}
