import time


def test_debug_performance(run_semgrep_in_tmp):
    """
    Verify that running semgrep with --debug does not result in
    performance slowdown wrt running without --debug
    """
    start_without_debug = time.time()
    run_semgrep_in_tmp("rules/long_message.yaml", target_name="simple.yaml")
    time_without_debug = time.time() - start_without_debug

    # If this fails the rule was probably optimized out
    assert time_without_debug > 2

    start_with_debug = time.time()
    run_semgrep_in_tmp(
        "rules/long_message.yaml",
        target_name="simple.yaml",
        options=["--debug", "--time"],
    )
    time_with_debug = time.time() - start_with_debug

    # There was a slowdown of more than 10% in reality the time should be basically the same
    # but we account for variablity in runtime here
    assert time_with_debug - time_without_debug < 0.1 * time_without_debug
