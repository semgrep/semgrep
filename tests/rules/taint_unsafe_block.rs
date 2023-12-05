struct Engine {
  fuel: u64,
}
struct Ship {
  engine: Box<Engine>,
}
fn main() {
    let mut ship = Ship { engine: Box::new(Engine { fuel: 42 }) };
    
    unsafe {
        let engine_ptr_ptr =
            std::mem::transmute::<&mut Box<Engine>, *mut u64>(&mut ship.engine);
        // Make the ship's .engine field point to address 0x1337.
        *engine_ptr_ptr = 0x1337;
    }
    // ruleid: test
    sink(ship.engine.fuel);
}
