// Test file for parentheses in comments across various directives
module github.com/test/parens

go 1.20

// Test tool directive with various parentheses patterns
tool (
	github.com/campoy/jsonenums // Used for generating (Un)MarshalJSON methods
	github.com/foo/bar // Multiple (parens) in (comment)
	github.com/baz/qux // Nested ((parentheses))
	github.com/nested/deeper // Even more (nested (parens) here)
	github.com/end/paren // Ends with paren (test)
)

// Test require directive with parentheses in comments
require (
	github.com/pkg/errors v0.9.1 // Error handling (wrapping)
	github.com/stretchr/testify v1.8.0 // Testing framework (assertions)
	github.com/go-chi/chi/v5 v5.0.7 // Router (HTTP)
)

// Test exclude directive with parentheses
exclude (
	github.com/bad/package v1.0.0 // Known issue (CVE-2023-1234)
	github.com/another/bad v2.0.0 // Security problem (deprecated)
)

// Test replace directive with parentheses in comments (with tab before closing paren)
replace (
	github.com/old/package v1.0.0 => github.com/new/package v2.0.0 // Migration (v1 to v2)
	github.com/fork/original v0.5.0 => github.com/fork/updated v0.6.0 // Fork (performance fixes)
	)

// Test retract directive with parentheses (with multiple spaces before closing paren)
retract (
	v1.0.0 // Bad release (broken build)
	[v1.1.0, v1.2.0] // Broken versions (regression in feature)
    )

// Test single-line directives with parentheses in comments
toolchain go1.21.0 // Toolchain version (latest stable)
godebug http2client=0 // HTTP/2 config (disabled)

// Test require with inline and multi-line (Un)MarshalJSON style comments
require github.com/json-iterator/go v1.1.12 // Fast JSON (Un)marshaling

// Edge cases: ensure the + quantifier (one or more) doesn't break valid cases
require (
	github.com/edge/case1 v1.0.0
	github.com/edge/case2 v2.0.0 // minimal comment
	github.com/edge/case3 v3.0.0 //
	github.com/edge/case4 v4.0.0 // a
	github.com/edge/case5 v5.0.0 // comment ending with paren)
	github.com/edge/case6 v6.0.0 // )
)

// Test tool without comments (ensure + quantifier doesn't require comments)
tool (
	github.com/tool/nocomment1
	github.com/tool/nocomment2
  )

// Test with closing paren immediately after comment paren)
exclude github.com/bad/pkg v1.0.0 // issue (#123)
