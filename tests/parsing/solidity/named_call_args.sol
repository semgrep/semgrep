pragma solidity ^0.8.24;

contract C {
    function f() public pure returns (uint256) {
        return f({a: 1, b: 2});
    }

    function f(uint256 a, uint256 b) public pure returns (uint256) {
        return a + b;
    }
}
