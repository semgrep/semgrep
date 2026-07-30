pragma solidity ^0.8.24;

library L {
    function add(uint256 a, uint256 b) internal pure returns (uint256) {
        return a + b;
    }

    function sub(uint256 a, uint256 b) internal pure returns (uint256) {
        return a - b;
    }
}

contract C {
    using {L.add, L.sub} for uint256;
}
