struct Ptr(&'static u8);
// MATCH: 
impl flatbuffers::SafeSliceAccess for Ptr {}

fn main() {
    let v = flatbuffers::Vector::<'_, Ptr>::new(
        &[1, 0, 0, 0, 0x41, 0x41, 0x41, 0x41, 0x41, 0x41, 0x41, 0x41],
        0,
    );
    let x = v.safe_slice()[0].0;
    println!("{}", *x);
}
