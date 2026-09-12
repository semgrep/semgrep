/**
 * This provides an example for sending locked coins to recipients to be unlocked after a specific time.
 *
 * Locked coins flow:
 * 1. Deploy the lockup contract. Deployer can decide if the contract is upgradable or not.
 * 2. Sponsor accounts (sponsors) call initialize_sponsor with the appropriate CoinType to set up their account for
 * creating locks later.
 * 2. Sponsors add locked APTs for custom expiration time + amount for recipients. Each lockup is called a "lock".
 * 3. Sponsors can revoke a lock or change lockup (reduce or extend) anytime. This gives flexibility in case of
 * contract violation or special circumstances. If this is not desired, the deployer can remove these functionalities
 * before deploying. If a lock is canceled, the locked coins will be sent back to the withdrawal address. This
 * withdrawal address is set when initilizing the sponsor account and can only be changed when there are no active or
 * unclaimed locks.
 * 4. Once the lockup has expired, the recipient can call claim to get the unlocked tokens.
 **/
module defi::locked_coins {
    use aptos_framework::coin::{Self, Coin};
    use aptos_framework::event;
    use aptos_framework::timestamp;
    use aptos_std::table::{Self, Table};
    use std::error;
    use std::signer;
    use std::vector;

    /// Represents a lock of coins until some specified unlock time. Afterward, the recipient can claim the coins.
    struct Lock<phantom CoinType> has store {
        coins: Coin<CoinType>,
        unlock_time_secs: u64,
    }

    /// Holder for a map from recipients => locks.
    /// There can be at most one lock per recipient.
    struct Locks<phantom CoinType> has key {
        // Map from recipient address => locked coins.
        locks: Table<address, Lock<CoinType>>,
        // Predefined withdrawal address. This cannot be changed if there's any active lock.
        withdrawal_address: address,
        // Number of locks that have not yet been claimed.
        total_locks: u64,
    }

    #[event]
    /// Event emitted when a lock is canceled.
    struct CancelLockup has drop, store {
        sponsor: address,
        recipient: address,
        amount: u64,
    }

    #[event]
    /// Event emitted when a recipient claims unlocked coins.
    struct Claim has drop, store {
        sponsor: address,
        recipient: address,
        amount: u64,
        claimed_time_secs: u64,
    }

    #[event]
    /// Event emitted when lockup is updated for an existing lock.
    struct UpdateLockup has drop, store {
        sponsor: address,
        recipient: address,
        old_unlock_time_secs: u64,
        new_unlock_time_secs: u64,
    }

    #[event]
    /// Event emitted when withdrawal address is updated.
    struct UpdateWithdrawalAddress has drop, store {
        sponsor: address,
        old_withdrawal_address: address,
        new_withdrawal_address: address,
    }

    /// No locked coins found to claim.
    const ELOCK_NOT_FOUND: u64 = 1;
    /// Lockup has not expired yet.
    const ELOCKUP_HAS_NOT_EXPIRED: u64 = 2;
    /// Can only create one active lock per recipient at once.
    const ELOCK_ALREADY_EXISTS: u64 = 3;
    /// The length of the recipients list doesn't match the amounts.
    const EINVALID_RECIPIENTS_LIST_LENGTH: u64 = 3;
    /// Sponsor account has not been set up to create locks for the specified CoinType yet.
    const ESPONSOR_ACCOUNT_NOT_INITIALIZED: u64 = 4;
    /// Cannot update the withdrawal address because there are still active/unclaimed locks.
    const EACTIVE_LOCKS_EXIST: u64 = 5;

    #[view]
    /// Return the total number of locks created by the sponsor for the given CoinType.
    public fun total_locks<CoinType>(sponsor: address): u64 acquires Locks {
        assert!(exists<Locks<CoinType>>(sponsor), error::not_found(ESPONSOR_ACCOUNT_NOT_INITIALIZED));
        let locks = borrow_global<Locks<CoinType>>(sponsor);
        locks.total_locks
    }

    #[view]
    /// Return the number of coins a sponsor has locked up for the given recipient.
    /// This throws an error if there are no locked coins setup for the given recipient.
    public fun locked_amount<CoinType>(sponsor: address, recipient: address): u64 acquires Locks {
        assert!(exists<Locks<CoinType>>(sponsor), error::not_found(ESPONSOR_ACCOUNT_NOT_INITIALIZED));
        let locks = borrow_global<Locks<CoinType>>(sponsor);
        assert!(table::contains(&locks.locks, recipient), error::not_found(ELOCK_NOT_FOUND));
        coin::value(&table::borrow(&locks.locks, recipient).coins)
    }

    #[view]
    /// Return the timestamp (in seconds) when the given recipient can claim coins locked up for them by the sponsor.
    /// This throws an error if there are no locked coins setup for the given recipient.
    public fun claim_time_secs<CoinType>(sponsor: address, recipient: address): u64 acquires Locks {
        assert!(exists<Locks<CoinType>>(sponsor), error::not_found(ESPONSOR_ACCOUNT_NOT_INITIALIZED));
        let locks = borrow_global<Locks<CoinType>>(sponsor);
        assert!(table::contains(&locks.locks, recipient), error::not_found(ELOCK_NOT_FOUND));
        table::borrow(&locks.locks, recipient).unlock_time_secs
    }

    #[view]
    /// Return the withdrawal address for a sponsor's locks (where canceled locks' funds are sent to).
    public fun withdrawal_address<CoinType>(sponsor: address): address acquires Locks {
        assert!(exists<Locks<CoinType>>(sponsor), error::not_found(ESPONSOR_ACCOUNT_NOT_INITIALIZED));
        let locks = borrow_global<Locks<CoinType>>(sponsor);
        locks.withdrawal_address
    }

    /// Initialize the sponsor account to allow creating locks.
    public entry fun initialize_sponsor<CoinType>(sponsor: &signer, withdrawal_address: address) {
        move_to(sponsor, Locks {
            locks: table::new<address, Lock<CoinType>>(),
            withdrawal_address,
            total_locks: 0,
        })
    }

    /// Update the withdrawal address. This is only allowed if there are currently no active locks.
    public entry fun update_withdrawal_address<CoinType>(
        sponsor: &signer, new_withdrawal_address: address) acquires Locks {
        let sponsor_address = signer::address_of(sponsor);
        assert!(exists<Locks<CoinType>>(sponsor_address), error::not_found(ESPONSOR_ACCOUNT_NOT_INITIALIZED));

        let locks = borrow_global_mut<Locks<CoinType>>(sponsor_address);
        assert!(locks.total_locks == 0, error::invalid_state(EACTIVE_LOCKS_EXIST));
        let old_withdrawal_address = locks.withdrawal_address;
        locks.withdrawal_address = new_withdrawal_address;

        event::emit(UpdateWithdrawalAddress {
            sponsor: sponsor_address,
            old_withdrawal_address,
            new_withdrawal_address,
        });
    }

    /// Batch version of add_locked_coins to process multiple recipients and corresponding amounts.
    public entry fun batch_add_locked_coins<CoinType>(
        sponsor: &signer, recipients: vector<address>, amounts: vector<u64>, unlock_time_secs: u64) acquires Locks {
        let len = vector::length(&recipients);
        assert!(len == vector::length(&amounts), error::invalid_argument(EINVALID_RECIPIENTS_LIST_LENGTH));
        vector::enumerate_ref(&recipients, |i, recipient| {
            let amount = *vector::borrow(&amounts, i);
            add_locked_coins<CoinType>(sponsor, *recipient, amount, unlock_time_secs);
        });
    }

    /// `Sponsor` can add locked coins for `recipient` with given unlock timestamp (in seconds).
    /// There's no restriction on unlock timestamp so sponsors could technically add coins for an unlocked time in the
    /// past, which means the coins are immediately unlocked.
    public entry fun add_locked_coins<CoinType>(
        sponsor: &signer, recipient: address, amount: u64, unlock_time_secs: u64) acquires Locks {
        let sponsor_address = signer::address_of(sponsor);
        assert!(exists<Locks<CoinType>>(sponsor_address), error::not_found(ESPONSOR_ACCOUNT_NOT_INITIALIZED));

        let locks = borrow_global_mut<Locks<CoinType>>(sponsor_address);
        let coins = coin::withdraw<CoinType>(sponsor, amount);
        assert!(!table::contains(&locks.locks, recipient), error::already_exists(ELOCK_ALREADY_EXISTS));
        table::add(&mut locks.locks, recipient, Lock<CoinType> { coins, unlock_time_secs });
        locks.total_locks = locks.total_locks + 1;
    }

    /// Recipient can claim coins that are fully unlocked (unlock time has passed).
    /// To claim, `recipient` would need the sponsor's address. In the case where each sponsor always deploys this
    /// module anew, it'd just be the module's hosted account address.
    public entry fun claim<CoinType>(recipient: &signer, sponsor: address) acquires Locks {
        assert!(exists<Locks<CoinType>>(sponsor), error::not_found(ESPONSOR_ACCOUNT_NOT_INITIALIZED));
        let locks = borrow_global_mut<Locks<CoinType>>(sponsor);
        let recipient_address = signer::address_of(recipient);
        assert!(table::contains(&locks.locks, recipient_address), error::not_found(ELOCK_NOT_FOUND));

        // Delete the lock entry both to keep records clean and keep storage usage minimal.
        // This would be reverted if validations fail later (transaction atomicity).
        let Lock { coins, unlock_time_secs } = table::remove(&mut locks.locks, recipient_address);
        locks.total_locks = locks.total_locks - 1;
        let now_secs = timestamp::now_seconds();
        assert!(now_secs >= unlock_time_secs, error::invalid_state(ELOCKUP_HAS_NOT_EXPIRED));

        let amount = coin::value(&coins);
        // This would fail if the recipient account is not registered to receive CoinType.
        coin::deposit(recipient_address, coins);

        event::emit(Claim {
            sponsor,
            recipient: recipient_address,
            amount,
            claimed_time_secs: now_secs,
        });
    }

    /// Batch version of update_lockup.
    public entry fun batch_update_lockup<CoinType>(
        sponsor: &signer, recipients: vector<address>, new_unlock_time_secs: u64) acquires Locks {
        let sponsor_address = signer::address_of(sponsor);
        assert!(exists<Locks<CoinType>>(sponsor_address), error::not_found(ESPONSOR_ACCOUNT_NOT_INITIALIZED));

        vector::for_each_ref(&recipients, |recipient| {
            update_lockup<CoinType>(sponsor, *recipient, new_unlock_time_secs);
        });
    }

    /// Sponsor can update the lockup of an existing lock.
    public entry fun update_lockup<CoinType>(
        sponsor: &signer, recipient: address, new_unlock_time_secs: u64) acquires Locks {
        let sponsor_address = signer::address_of(sponsor);
        assert!(exists<Locks<CoinType>>(sponsor_address), error::not_found(ESPONSOR_ACCOUNT_NOT_INITIALIZED));
        let locks = borrow_global_mut<Locks<CoinType>>(sponsor_address);
        assert!(table::contains(&locks.locks, recipient), error::not_found(ELOCK_NOT_FOUND));

        let lock = table::borrow_mut(&mut locks.locks, recipient);
        let old_unlock_time_secs = lock.unlock_time_secs;
        lock.unlock_time_secs = new_unlock_time_secs;

        event::emit(UpdateLockup {
            sponsor: sponsor_address,
            recipient,
            old_unlock_time_secs,
            new_unlock_time_secs,
        });
    }

    /// Batch version of cancel_lockup to cancel the lockup for multiple recipients.
    public entry fun batch_cancel_lockup<CoinType>(sponsor: &signer, recipients: vector<address>) acquires Locks {
        let sponsor_address = signer::address_of(sponsor);
        assert!(exists<Locks<CoinType>>(sponsor_address), error::not_found(ESPONSOR_ACCOUNT_NOT_INITIALIZED));

        vector::for_each_ref(&recipients, |recipient| {
            cancel_lockup<CoinType>(sponsor, *recipient);
        });
    }

    /// Sponsor can cancel an existing lock.
    public entry fun cancel_lockup<CoinType>(sponsor: &signer, recipient: address) acquires Locks {
        let sponsor_address = signer::address_of(sponsor);
        assert!(exists<Locks<CoinType>>(sponsor_address), error::not_found(ESPONSOR_ACCOUNT_NOT_INITIALIZED));
        let locks = borrow_global_mut<Locks<CoinType>>(sponsor_address);
        assert!(table::contains(&locks.locks, recipient), error::not_found(ELOCK_NOT_FOUND));

        // Remove the lock and deposit coins backed into the sponsor account.
        let Lock { coins, unlock_time_secs: _ } = table::remove(&mut locks.locks, recipient);
        locks.total_locks = locks.total_locks - 1;
        let amount = coin::value(&coins);
        coin::deposit(locks.withdrawal_address, coins);

        event::emit(
            CancelLockup {
                sponsor: sponsor_address,
                recipient,
                amount
            });
    }

    #[test_only]
    use std::string;
    #[test_only]
    use aptos_framework::account;
    #[test_only]
    use aptos_framework::coin::BurnCapability;
    #[test_only]
    use aptos_framework::aptos_coin::AptosCoin;
    #[test_only]
    use aptos_framework::aptos_account;

    #[test_only]
    fun setup(aptos_framework: &signer, sponsor: &signer): BurnCapability<AptosCoin> {
        timestamp::set_time_has_started_for_testing(aptos_framework);

        let (burn_cap, freeze_cap, mint_cap) = coin::initialize<AptosCoin>(
            aptos_framework,
            string::utf8(b"TC"),
            string::utf8(b"TC"),
            8,
            false,
        );
        account::create_account_for_test(signer::address_of(sponsor));
        coin::register<AptosCoin>(sponsor);
        let coins = coin::mint<AptosCoin>(2000, &mint_cap);
        coin::deposit(signer::address_of(sponsor), coins);
        coin::destroy_mint_cap(mint_cap);
        coin::destroy_freeze_cap(freeze_cap);

        burn_cap
    }

    #[test(aptos_framework = @0x1, sponsor = @0x123, recipient = @0x234)]
    public entry fun test_recipient_can_claim_coins(
        aptos_framework: &signer, sponsor: &signer, recipient: &signer) acquires Locks {
        let burn_cap = setup(aptos_framework, sponsor);
        let recipient_addr = signer::address_of(recipient);
        aptos_account::create_account(recipient_addr);
        let sponsor_address = signer::address_of(sponsor);
        initialize_sponsor<AptosCoin>(sponsor, sponsor_address);
        add_locked_coins<AptosCoin>(sponsor, recipient_addr, 1000, 1000);
        assert!(total_locks<AptosCoin>(sponsor_address) == 1, 0);
        timestamp::fast_forward_seconds(1000);
        claim<AptosCoin>(recipient, sponsor_address);
        assert!(total_locks<AptosCoin>(sponsor_address) == 0, 1);
        assert!(coin::balance<AptosCoin>(recipient_addr) == 1000, 0);
        coin::destroy_burn_cap(burn_cap);
    }

    #[test(aptos_framework = @0x1, sponsor = @0x123, recipient = @0x234)]
    #[expected_failure(abort_code = 0x30002, location = Self)]
    public entry fun test_recipient_cannot_claim_coins_if_lockup_has_not_expired(
        aptos_framework: &signer, sponsor: &signer, recipient: &signer) acquires Locks {
        let burn_cap = setup(aptos_framework, sponsor);
        let recipient_addr = signer::address_of(recipient);
        aptos_account::create_account(recipient_addr);
        let sponsor_address = signer::address_of(sponsor);
        initialize_sponsor<AptosCoin>(sponsor, sponsor_address);
        add_locked_coins<AptosCoin>(sponsor, recipient_addr, 1000, 1000);
        timestamp::fast_forward_seconds(500);
        claim<AptosCoin>(recipient, sponsor_address);
        coin::destroy_burn_cap(burn_cap);
    }

    #[test(aptos_framework = @0x1, sponsor = @0x123, recipient = @0x234)]
    #[expected_failure(abort_code = 0x60001, location = Self)]
    public entry fun test_recipient_cannot_claim_twice(
        aptos_framework: &signer, sponsor: &signer, recipient: &signer) acquires Locks {
        let burn_cap = setup(aptos_framework, sponsor);
        let recipient_addr = signer::address_of(recipient);
        aptos_account::create_account(recipient_addr);
        let sponsor_address = signer::address_of(sponsor);
        initialize_sponsor<AptosCoin>(sponsor, sponsor_address);
        add_locked_coins<AptosCoin>(sponsor, recipient_addr, 1000, 1000);
        timestamp::fast_forward_seconds(1000);
        claim<AptosCoin>(recipient, sponsor_address);
        claim<AptosCoin>(recipient, sponsor_address);
        coin::destroy_burn_cap(burn_cap);
    }

    #[test(aptos_framework = @0x1, sponsor = @0x123, recipient = @0x234)]
    public entry fun test_sponsor_can_update_lockup(
        aptos_framework: &signer, sponsor: &signer, recipient: &signer) acquires Locks {
        let burn_cap = setup(aptos_framework, sponsor);
        let recipient_addr = signer::address_of(recipient);
        aptos_account::create_account(recipient_addr);
        let sponsor_address = signer::address_of(sponsor);
        initialize_sponsor<AptosCoin>(sponsor, sponsor_address);
        add_locked_coins<AptosCoin>(sponsor, recipient_addr, 1000, 1000);
        assert!(total_locks<AptosCoin>(sponsor_address) == 1, 0);
        assert!(claim_time_secs<AptosCoin>(sponsor_address, recipient_addr) == 1000, 0);
        // Extend lockup.
        update_lockup<AptosCoin>(sponsor, recipient_addr, 2000);
        assert!(claim_time_secs<AptosCoin>(sponsor_address, recipient_addr) == 2000, 1);
        // Reduce lockup.
        update_lockup<AptosCoin>(sponsor, recipient_addr, 1500);
        assert!(claim_time_secs<AptosCoin>(sponsor_address, recipient_addr) == 1500, 2);
        assert!(total_locks<AptosCoin>(sponsor_address) == 1, 1);

        coin::destroy_burn_cap(burn_cap);
    }

    #[test(aptos_framework = @0x1, sponsor = @0x123, recipient_1 = @0x234, recipient_2 = @0x345)]
    public entry fun test_sponsor_can_batch_update_lockup(
        aptos_framework: &signer, sponsor: &signer, recipient_1: &signer, recipient_2: &signer) acquires Locks {
        let burn_cap = setup(aptos_framework, sponsor);
        let sponsor_addr = signer::address_of(sponsor);
        let recipient_1_addr = signer::address_of(recipient_1);
        let recipient_2_addr = signer::address_of(recipient_2);
        aptos_account::create_account(recipient_1_addr);
        aptos_account::create_account(recipient_2_addr);
        let sponsor_address = signer::address_of(sponsor);
        initialize_sponsor<AptosCoin>(sponsor, sponsor_address);
        batch_add_locked_coins<AptosCoin>(
            sponsor,
            vector[recipient_1_addr, recipient_2_addr],
            vector[1000, 1000],
            1000
        );
        assert!(claim_time_secs<AptosCoin>(sponsor_addr, recipient_1_addr) == 1000, 0);
        assert!(claim_time_secs<AptosCoin>(sponsor_addr, recipient_2_addr) == 1000, 0);
        // Extend lockup.
        batch_update_lockup<AptosCoin>(sponsor, vector[recipient_1_addr, recipient_2_addr], 2000);
        assert!(claim_time_secs<AptosCoin>(sponsor_addr, recipient_1_addr) == 2000, 1);
        assert!(claim_time_secs<AptosCoin>(sponsor_addr, recipient_2_addr) == 2000, 1);
        // Reduce lockup.
        batch_update_lockup<AptosCoin>(sponsor, vector[recipient_1_addr, recipient_2_addr], 1500);
        assert!(claim_time_secs<AptosCoin>(sponsor_addr, recipient_1_addr) == 1500, 2);
        assert!(claim_time_secs<AptosCoin>(sponsor_addr, recipient_2_addr) == 1500, 2);

        coin::destroy_burn_cap(burn_cap);
    }

    #[test(aptos_framework = @0x1, sponsor = @0x123, recipient = @0x234, withdrawal = @0x345)]
    public entry fun test_sponsor_can_cancel_lockup(
        aptos_framework: &signer, sponsor: &signer, recipient: &signer, withdrawal: &signer) acquires Locks {
        let burn_cap = setup(aptos_framework, sponsor);
        let recipient_addr = signer::address_of(recipient);
        let withdrawal_addr = signer::address_of(withdrawal);
        aptos_account::create_account(withdrawal_addr);
        aptos_account::create_account(recipient_addr);
        let sponsor_address = signer::address_of(sponsor);
        initialize_sponsor<AptosCoin>(sponsor, withdrawal_addr);
        add_locked_coins<AptosCoin>(sponsor, recipient_addr, 1000, 1000);
        assert!(total_locks<AptosCoin>(sponsor_address) == 1, 0);
        assert!(coin::balance<AptosCoin>(withdrawal_addr) == 0, 0);
        cancel_lockup<AptosCoin>(sponsor, recipient_addr);
        assert!(total_locks<AptosCoin>(sponsor_address) == 0, 0);
        let locks = borrow_global_mut<Locks<AptosCoin>>(sponsor_address);
        assert!(!table::contains(&locks.locks, recipient_addr), 0);

        // Funds from canceled locks should be sent to the withdrawal address.
        assert!(coin::balance<AptosCoin>(withdrawal_addr) == 1000, 0);

        coin::destroy_burn_cap(burn_cap);
    }

    #[test(aptos_framework = @0x1, sponsor = @0x123, recipient_1 = @0x234, recipient_2 = @0x345, withdrawal = @0x456)]
    public entry fun test_sponsor_can_batch_cancel_lockup(
        aptos_framework: &signer,
        sponsor: &signer,
        recipient_1: &signer,
        recipient_2: &signer,
        withdrawal: &signer,
    ) acquires Locks {
        let burn_cap = setup(aptos_framework, sponsor);
        let recipient_1_addr = signer::address_of(recipient_1);
        let recipient_2_addr = signer::address_of(recipient_2);
        let withdrawal_addr = signer::address_of(withdrawal);
        aptos_account::create_account(recipient_1_addr);
        aptos_account::create_account(recipient_2_addr);
        aptos_account::create_account(withdrawal_addr);
        let sponsor_address = signer::address_of(sponsor);
        initialize_sponsor<AptosCoin>(sponsor, withdrawal_addr);
        batch_add_locked_coins<AptosCoin>(
            sponsor,
            vector[recipient_1_addr, recipient_2_addr],
            vector[1000, 1000],
            1000
        );
        batch_cancel_lockup<AptosCoin>(sponsor, vector[recipient_1_addr, recipient_2_addr]);
        let locks = borrow_global_mut<Locks<AptosCoin>>(sponsor_address);
        assert!(!table::contains(&locks.locks, recipient_1_addr), 0);
        assert!(!table::contains(&locks.locks, recipient_2_addr), 0);
        // Funds from canceled locks should be sent to the withdrawal address.
        assert!(coin::balance<AptosCoin>(withdrawal_addr) == 2000, 0);
        coin::destroy_burn_cap(burn_cap);
    }

    #[test(aptos_framework = @0x1, sponsor = @0x123, recipient = @0x234, withdrawal = @0x456)]
    #[expected_failure(abort_code = 0x30005, location = Self)]
    public entry fun test_cannot_change_withdrawal_address_if_active_locks_exist(
        aptos_framework: &signer,
        sponsor: &signer,
        recipient: &signer,
        withdrawal: &signer,
    ) acquires Locks {
        let burn_cap = setup(aptos_framework, sponsor);
        let recipient_addr = signer::address_of(recipient);
        let withdrawal_addr = signer::address_of(withdrawal);
        aptos_account::create_account(recipient_addr);
        aptos_account::create_account(withdrawal_addr);
        let sponsor_address = signer::address_of(sponsor);
        initialize_sponsor<AptosCoin>(sponsor, withdrawal_addr);
        add_locked_coins<AptosCoin>(sponsor, recipient_addr, 1000, 1000);
        update_withdrawal_address<AptosCoin>(sponsor, sponsor_address);
        coin::destroy_burn_cap(burn_cap);
    }

    #[test(aptos_framework = @0x1, sponsor = @0x123, recipient = @0x234, withdrawal = @0x456)]
    public entry fun test_can_change_withdrawal_address_if_no_active_locks_exist(
        aptos_framework: &signer,
        sponsor: &signer,
        recipient: &signer,
        withdrawal: &signer,
    ) acquires Locks {
        let burn_cap = setup(aptos_framework, sponsor);
        let recipient_addr = signer::address_of(recipient);
        let withdrawal_addr = signer::address_of(withdrawal);
        aptos_account::create_account(recipient_addr);
        aptos_account::create_account(withdrawal_addr);
        let sponsor_address = signer::address_of(sponsor);
        initialize_sponsor<AptosCoin>(sponsor, withdrawal_addr);
        assert!(withdrawal_address<AptosCoin>(sponsor_address) == withdrawal_addr, 0);
        add_locked_coins<AptosCoin>(sponsor, recipient_addr, 1000, 1000);
        cancel_lockup<AptosCoin>(sponsor, recipient_addr);
        update_withdrawal_address<AptosCoin>(sponsor, sponsor_address);
        assert!(withdrawal_address<AptosCoin>(sponsor_address) == sponsor_address, 0);
        coin::destroy_burn_cap(burn_cap);
    }
}