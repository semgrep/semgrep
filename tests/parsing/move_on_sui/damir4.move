module beep::boop {
    fun index_expression() {
        // incorrectly parsed as `(copy data)[i]`
        // (dot_expression [3, 8] - [3, 37]
        //   expr: (name_expression [3, 8] - [3, 14]
        //     access: (module_access [3, 8] - [3, 14]
        //       member: (identifier [3, 8] - [3, 14])))
        //   access: (index_expression [3, 15] - [3, 37] !!!! Should be `move_or_copy_expression`
        //     expr: (call_expression [3, 15] - [3, 34]
        //       access: (module_access [3, 15] - [3, 24]
        //         member: (identifier [3, 15] - [3, 24]))
        //       args: (arg_list [3, 24] - [3, 34]
        //         (move_or_copy_expression [3, 25] - [3, 34]
        //           expr: (variable_identifier [3, 30] - [3, 34]))))
        //     idx: (name_expression [3, 35] - [3, 36]
        //       access: (module_access [3, 35] - [3, 36]
        //         member: (identifier [3, 35] - [3, 36])))))
        result.push_back(copy data[i]);
    }
}
