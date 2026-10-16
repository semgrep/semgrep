locals {
  lead     = foo("  x")
  both     = foo("  x  ")
  trail    = foo("x  ")
  ws_only  = foo("   ")
}
