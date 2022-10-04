# ERROR:
if [[ $# -gt 0 ]]; then
  usage
  exit
fi

foo() {
  # ERROR:
  if test -z "$1"; then
    return 1
  fi
  echo "$1"
}
