contract MyContract {
    enum Status {
        BLOCKED,
        ACTIVATED
    }
    //ERROR: match
    event UserUnblocked(address indexed userAddress_);
}
