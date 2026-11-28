// Every branch of a preprocessor conditional may be compiled, so all of
// them must reach the AST, not only the #if branch.
#if DEBUG
class TopIf { void M() { } }
#else
class TopElse {
    void M() {
        // ruleid: sink-source
        sink(source());
    }
}
#endif

class C {
    void Body() {
#if DEBUG
        sink("debug");
#elif TRACE
        // ruleid: sink-source
        sink(source());
#else
        // ruleid: sink-source
        sink(source());
#endif
    }

#if DEBUG
    void MemberIf() { }
#else
    void MemberElse() {
        // ruleid: sink-source
        sink(source());
    }
#endif

    void Expr() {
        var x = 1 +
#if A
            f(2)
#else
            // ruleid: sink-source
            sink(source())
#endif
            ;
    }

    void Initializer() {
        var xs = new List<object> {
#if A
            f(1),
#else
            // ruleid: sink-source
            sink(source()),
#endif
        };
    }

    void Collection() {
        int[] xs = [
#if A
            f(1),
#else
            // ruleid: sink-source
            sink(source()),
#endif
        ];
    }

    object ReturnTail(bool c) {
        return c ? f(1) :
#if A
            f(2);
#else
            // ruleid: sink-source
            sink(source());
#endif
    }

    void ElseStmt(bool a) {
        if (a) { }
#if A
        else { f(1); }
#else
        else {
            // ruleid: sink-source
            sink(source());
        }
#endif
    }

    void FuncBody()
#if A
    { f(1); }
#else
    {
        // ruleid: sink-source
        sink(source());
    }
#endif

    // A finding starts at the method's first attribute.
#if ATTR
    // ruleid: obsolete-method
    [Obsolete]
#endif
    void Guarded() { }

    // The attributes of both branches are kept on the method: an
    // over-approximation (no build has both), so that each is visible.
#if ATTR
    // ruleid: obsolete-method
    [Serializable]
#else
    [Obsolete]
#endif
    void GuardedElse() { }
}
