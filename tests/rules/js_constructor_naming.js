@Injectable
class ClassName {
  foo() {
    // ruleid: js-constructor-naming
    return this.http.thing;
  }

  constructor(http: Ty) {}

  bar() {
    // ruleid: js-constructor-naming
    return this.http;
  }
}

@Component
class ClassName {
  foo() {
    // ruleid: js-constructor-naming
    return this.http.thing;
  }

  constructor(http: Ty) {}

  bar() {
    // ruleid: js-constructor-naming
    return this.http;
  }
}

// not injectable
class ClassName {
  foo() {
    return this.http.thing;
  }

  constructor(http: Ty) {}

  bar() {
    return this.http;
  }
}
