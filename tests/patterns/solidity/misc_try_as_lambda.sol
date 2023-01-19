pragma solidity 0.8.16;

interface D {
  function foo(uint256 x) external;
  function baz() external;
}

contract C {
  function bar(D d) external returns (uint256 r) {
    //ERROR: match
    try d.foo(42) {
      return 0;
    } catch {
      return 1;
    }
  }
}
contract D {
  function bar(D d) external returns (uint256 r) {
   // this used to (wrongly) match because we were discarding the expression passed to try
    try d.baz() {
      return 0;
    } catch {
      return 1;
    }
  }
}