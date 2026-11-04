package groups

type (
  // ERROR: match
  First = string
  Defined string
  Other = int
)
// ERROR: match
type (Last = string)
type ()
