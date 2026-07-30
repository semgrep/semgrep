pragma solidity ^0.8.24;

library L {
    function double(uint256 x) internal pure returns (uint256) {
        return x * 2;
    }
}

using L for uint256;
