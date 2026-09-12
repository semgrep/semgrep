module hello_sui::hello {
    public fun say_hello() {
        let message = b"Hello, World!";
        // Print the message
        std::debug::print(message);
    }
}
