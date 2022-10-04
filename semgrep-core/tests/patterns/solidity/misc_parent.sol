// from https://semgrep.dev/s/7nk2
pragma solidity ^0.6.0;

import "@openzeppelin/contracts-upgradeable/proxy/utils/Initializable.sol";

//ERROR: match
contract MyContract is Initializable {
    uint256 public x;

    function initialize(uint256 _x) public initializer {
        x = _x;
    }
}