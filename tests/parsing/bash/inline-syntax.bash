# Constructs that previously failed to parse
loop=; variables=& here=
case x in x) echo meow ;; esac
[[ abc != +(a|b|c) ]] && echo 2
[ ! command -v go &>/dev/null ] && return
echo $((n < 10 ? n : 10))
