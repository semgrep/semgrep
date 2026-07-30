pragma solidity ^0.8.24;

library L {
    function div(uint256 a, uint256 b) internal pure returns (uint256) {
        return a / b;
    }
}

contract C {
    function f(bool cond, uint256 x, uint256 y, uint256 z) public pure returns (uint256) {
        return cond ? x : y.div(z);
    }
}
