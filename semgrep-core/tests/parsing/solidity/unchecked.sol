pragma solidity ^0.8.7;

contract C {
    uint256 public s;

    function func(uint128 v) public {
        unchecked {
            s = s - v;
        }

        (bool r, ) = msg.sender.call{value: v}("");
        require(r, " transfer ");
    }
}
