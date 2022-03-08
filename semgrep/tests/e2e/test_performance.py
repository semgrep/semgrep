import time


def test_debug_performance(run_semgrep_in_tmp):
    """
    Verify that running semgrep with --debug does not result in
    performance slowdown wrt running without --debug
    """
    start_without_debug = time.time()
    run_semgrep_in_tmp("rules/long_message.yaml", target_name="simple.yaml")
    time_without_debug = time.time() - start_without_debug

    assert (
        time_without_debug > 2
    ), "runtime was suspiciously low, was the rule optimized out?"

    start_with_debug = time.time()
    run_semgrep_in_tmp(
        "rules/long_message.yaml",
        target_name="simple.yaml",
        options=["--debug", "--time"],
    )
    time_with_debug = time.time() - start_with_debug

    from pytest import approx

    assert time_with_debug == approx(
        time_without_debug, rel=0.1
    ), "adding --debug slowed runtime by more than 10%"
