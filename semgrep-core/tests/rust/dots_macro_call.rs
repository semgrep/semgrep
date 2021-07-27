use log::debug;
use env_logger;
fn main() {

    env_logger::init();
    //ERROR: match
    println!("hello");
    //ERROR: match
    println!("other");
    //ERROR: match
    println!("other", stuff);
}
