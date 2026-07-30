pragma solidity ^0.8.24;

contract C {
    function f() public view returns (uint256, uint256, uint256, uint256, uint256, uint256) {
        assembly {
            let a := basefee()
            let b := prevrandao()
            let c := blobbasefee()
            let d := blobhash(0)
            mcopy(0, 0, 32)
        }
        return (block.basefee, block.prevrandao, block.blobbasefee, block.blobhash(0), 0, 0);
    }
}
