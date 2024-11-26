export class Test {
  //ERROR:
  match1(x, @Bar() y, z): string {
    return '';
  }

  //ERROR:
  match2(@Bar() y, z): string {
    return '';
  }

  //ERROR:
  match3(x, @Bar() y): string {
    return '';
  }

  //ERROR:
  match4(@Bar() y): string {
    return '';
  }

  //ERROR:
  match5(@Bar() y, a, b, c, d, e): string {
    return '';
  }

  no_match(x, @Abc() y, z): string {
    return '';
  }
}
