# ERROR: match
import A: @m as @B.n
# ERROR: match
import A: x as y, @m as @B.n
import A: @m as @n
import A: @m as @C.n
import A: m as @B.n
