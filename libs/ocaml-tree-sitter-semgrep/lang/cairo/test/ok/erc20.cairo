use openzeppelin::token::erc20::ERC20;
use starknet::contract_address_const;
use starknet::ContractAddress;
use starknet::testing::set_caller_address;
use integer::u256;
use integer::u256_from_felt252;
use integer::BoundedInt;
use traits::Into;

//
// Constants
//

const NAME: felt252 = 111;
const SYMBOL: felt252 = 222;

//
// Helper functions
//

fn setup() -> (ContractAddress, u256) {
    let initial_supply: u256 = u256_from_felt252(2000);
    let account: ContractAddress = contract_address_const::<1>();
    // Set account as default caller
    set_caller_address(account);

    ERC20::constructor(NAME, SYMBOL, initial_supply, account);
    (account, initial_supply)
}

fn set_caller_as_zero() {
    set_caller_address(contract_address_const::<0>());
}

//
// Tests
//

#[test]
#[available_gas(2000000)]
fn test_initializer() {
    ERC20::initializer(NAME, SYMBOL);

    assert(ERC20::name() == NAME, 'Name should be NAME');
    assert(ERC20::symbol() == SYMBOL, 'Symbol should be SYMBOL');
    assert(ERC20::decimals() == 18_u8, 'Decimals should be 18');
    assert(ERC20::total_supply() == u256_from_felt252(0), 'Supply should eq 0');
}


#[test]
#[available_gas(2000000)]
fn test_constructor() {
    let initial_supply: u256 = u256_from_felt252(2000);
    let account: ContractAddress = contract_address_const::<1>();
    let decimals: u8 = 18_u8;

    ERC20::constructor(NAME, SYMBOL, initial_supply, account);

    let owner_balance: u256 = ERC20::balance_of(account);
    assert(owner_balance == initial_supply, 'Should eq inital_supply');

    assert(ERC20::total_supply() == initial_supply, 'Should eq inital_supply');
    assert(ERC20::name() == NAME, 'Name should be NAME');
    assert(ERC20::symbol() == SYMBOL, 'Symbol should be SYMBOL');
    assert(ERC20::decimals() == decimals, 'Decimals should be 18');
}

#[test]
#[available_gas(2000000)]
fn test_approve() {
    let (owner, supply) = setup();
    let spender: ContractAddress = contract_address_const::<2>();
    let amount: u256 = u256_from_felt252(100);

    let success: bool = ERC20::approve(spender, amount);
    assert(success, 'Should return true');
    assert(ERC20::allowance(owner, spender) == amount, 'Spender not approved correctly');
}

#[test]
#[available_gas(2000000)]
#[should_panic(expected: ('ERC20: approve from 0', ))]
fn test_approve_from_zero() {
    let (owner, supply) = setup();
    let spender: ContractAddress = contract_address_const::<2>();
    let amount: u256 = u256_from_felt252(100);

    set_caller_as_zero();

    ERC20::approve(spender, amount);
}

#[test]
#[available_gas(2000000)]
#[should_panic(expected: ('ERC20: approve to 0', ))]
fn test_approve_to_zero() {
    let (owner, supply) = setup();
    let spender: ContractAddress = contract_address_const::<0>();
    let amount: u256 = u256_from_felt252(100);

    ERC20::approve(spender, amount);
}
