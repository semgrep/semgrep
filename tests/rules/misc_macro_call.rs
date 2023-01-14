use log::debug;
use env_logger;
fn main() {
    env_logger::init();
    //ruleid: test-macro-match 
    println!("Hello, world!");
    //ruleid: test-macro-match 
    debug!("test");
}
