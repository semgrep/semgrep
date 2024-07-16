module 0xadd::ress {
    // ruleid: pattern_move_use_member
    use something::pack::{Self, unsafe};

    // ruleid: pattern_move_const_member
    const this_is_a_test: address = @42;
}

script {
    // ruleid: pattern_move_use_member
    use something::pack::unsafe;

    // ruleid: pattern_move_const_member
    const script_const: address = @0xabcd;
}