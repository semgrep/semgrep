// ruleid: qualified-ellipsis, qualified-ellipsis-metavar
(ns/foo 1)

// ruleid: qualified-ellipsis, qualified-ellipsis-metavar
(ns/bar 2 3)

// ruleid: qualified-ellipsis, qualified-ellipsis-metavar
(ns/a/b 4)

// ruleid: qualified-ellipsis-metavar
(other/foo 1)

(foo 1)

// todoruleid: qualified-ellipsis-args
(ns/foo)

// todoruleid: qualified-ellipsis-args
(ns/bar)

(other/name)

(ns)
