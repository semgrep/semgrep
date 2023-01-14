# what actually matters in this test is the value bound to $X
# in the .sgrep. It should be the full path!
# use semgrep-core ... -pvar '$X' to print the value.

#ERROR: match
import a
#ERROR: match
import auth
#ERROR: match
import arbitrary_auth
#ERROR: match
import a.auth
#ERROR: match
import b.auth
#ERROR: match
import a.b.auth
