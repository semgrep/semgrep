use log::debug;
use env_logger;
fn main() {
    //ERROR: match
    env_logger::init();
    //ERROR: match
    println!("Hello, world!");
    //ERROR: match
    debug!("test");
}
