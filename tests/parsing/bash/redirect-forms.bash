# a heredoc followed by '&&': the heredoc applies to 'cat'
cat <<END && echo ok
body
END
# a here-string among the arguments
grep x <<< "$v" -i
# a statement made only of a redirect
> out.txt
# closing a file descriptor; 'foo' is an ordinary argument
exec 3>&- foo
# arguments after a prefix redirect come first: the command is 'foo'
>out foo echo hi
# a pipeline run in the background
a | b &
c
