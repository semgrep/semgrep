class Example {
    // MATCH:
    @log
    sum() : string {
      return a + b;
    }
  }

class Example {
    @log('something')
    sum() : string {
      return a + b;
    }
  }