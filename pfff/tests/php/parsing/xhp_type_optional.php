<?hh

class Test {
  private int $type;
  private :x:frag $xhp1;
  // this can potentially be ambiguous with the ?: empty ternary
  // PHP extension.
  private ?:x:frag $xhp1;
}