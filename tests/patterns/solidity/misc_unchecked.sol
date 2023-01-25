pragma solidity 0.8.16;

contract C {
  function foo(uint256 x, uint256 y) external returns (uint256 z) {
    z = 0;
    //ERROR: match
    unchecked {
      z = x + y;
    }
    z = x + y;
    return z;
  }
}
