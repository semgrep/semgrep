## [1.88.0](https://github.com/returntocorp/semgrep/releases/tag/v1.88.0) - 2024-09-18


### Added


- The dataflow analysis in the Pro engine can now track method invocations on
  variables of an interface type, safely assuming that any implementation of the
  method can be called. For example, tainted input vulnerabilities in both
  implementation classes can now be detected in the following code:

  ```java
  public interface MovieService {
    String vulnerableInjection(String input);
  }

  public class SimpleImpl implements MovieService {
    @Override
    public String vulnerableInjection(String input) {
      return sink(input);
    }
  }

  public class MoreImpl implements MovieService {
    @Override
    public String vulnerableInjection(String input) {
      return sink(input);
    }
  }

  public class AppController {
    private MovieService movieService;

    public String pwnTest(String taintedInput) {
      return movieService.vulnerableInjection(taintedInput);
    }
  }
  ``` (code-7435)
- Type inference for constructor parameter properties in TypeScript is now
  supported in the Pro engine. For example, the taint analysis can recognize that
  `sampleFunction` is defined in `AbstractedService` class in the following code:

  ```
  export class AppController {
      constructor(private readonly abstractedService: AbstractedService) {}

      async taintTest() {
          const src = source();
          await this.abstractedService.sampleFunction(src);
      }
  }
  ``` (code-7597)


### Changed


- include the exit code that semgrep will emit in the fail-open payload prior to exiting with a failure. (gh-2033)
