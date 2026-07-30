pragma solidity ^0.8.24;

contract C {
    uint256 transient x;

    function f() public {
        assembly {
            tstore(x.slot, 1)
            let v := tload(x.slot)
        }
    }
}
