pragma solidity ^0.8.24;

contract C {
    function f() public {
        assembly {
            let x := hex"dead_beef"
        }
    }
}
