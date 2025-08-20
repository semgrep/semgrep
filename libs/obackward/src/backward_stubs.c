#include <caml/mlvalues.h>

int register_sh();

CAMLprim value ml_register(value v_unit) {
    int exit_code;
    // Register the signal handler for segmentation faults
    //
    // Sometimes it can fail, I'm not sure why, but if it does let's print an
    // exit code
    if ((exit_code = register_sh()) != 1) {
        fprintf(stderr,
                "Failed to register segfault signal handler! exit_code: %d\n",
                exit_code);
    }
    return Val_bool(exit_code == 1);
}
