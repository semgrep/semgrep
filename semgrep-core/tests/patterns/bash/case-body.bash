case "$1" in
  --help)
    # MATCH:
    echo "hello"
    ;;
  *)
    # MATCH:
    echo "invalid argument: $1"
esac
