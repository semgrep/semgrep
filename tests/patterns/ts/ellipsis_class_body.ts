// ERROR: match
@Controller("/foo/bar")
export class FooController {
  test1() {
    doSomething1();
  }

  @Post("/foo")
  async test2(@Body() body: FooRequestBody) {
    doSomething2(body.foo);
  }

  test3() {
    doSomething3();
  }
}
