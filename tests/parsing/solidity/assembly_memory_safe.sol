pragma solidity ^0.8.24;

contract C {
    function f() public {
        assembly ("memory-safe") {
            let x := 1
        }
    }
}
