# Text after a heredoc delimiter continues the redirected command
cat <<EOF arg1 arg2
hello $USER
EOF
cat <<EOF | grep hello
hello
EOF
cat <<EOF > out.txt && echo written
hello
EOF
