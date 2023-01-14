pragma solidity 0.8.16;

contract C {
  modifier foo() { _; }
  //ERROR: match
  constructor() { }
}