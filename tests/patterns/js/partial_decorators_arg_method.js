class Example {
    @log
    sum() : string {
      return a + b;
    }
  }

class Example {
    // MATCH:
    @log('something')
    sum() : string {
      return a + b;
    }
  }