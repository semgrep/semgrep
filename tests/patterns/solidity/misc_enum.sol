contract MyContract {
    //ERROR: match
    enum Status {
        BLOCKED,
        ACTIVATED
    }
    event UserUnblocked(address indexed userAddress_);
}