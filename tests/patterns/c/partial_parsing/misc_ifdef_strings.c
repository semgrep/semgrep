void main() {


  //ERROR: match
  SOME_FUNC(
    setexpr,
    "some text",
    "more text\n"
    "other text"
#ifdef CONFIG_REGEX
    "\n"
    "even more text"
#endif
  );

}

