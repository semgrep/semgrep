# Heredoc delimiters with special characters, and a here-string on an fd
node <<_DELIMITER_WITH_UNDERSCORES_
  hello
_DELIMITER_WITH_UNDERSCORES_
node <<'```'
  hello
```
node <<!HEREDOC!
  hello
!HEREDOC!
while read -r x; do echo "$x"; done 3<<<"$ENTRIES"
