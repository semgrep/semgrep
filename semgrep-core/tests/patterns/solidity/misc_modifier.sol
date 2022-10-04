pragma solidity 0.8.16;

contract C {
  //ERROR: match
  modifier foo() { _; }
  constructor() { }
}