import time

import pytest


@pytest.mark.slow
@pytest.mark.flaky(reruns=2)
def test_debug_performance(run_semgrep_in_tmp):
    """
    Verify that running semgrep with --debug does not result in
    performance slowdown wrt running without --debug
    """
    start_without_debug = time.time()
    run_semgrep_in_tmp("rules/long_message.yaml", target_name="simple.yaml")
    time_without_debug = time.time() - start_without_debug

    assert (
        time_without_debug > 0.8
    ), "runtime was suspiciously low, was the rule optimized out?"

    start_with_debug = time.time()
    run_semgrep_in_tmp(
        "rules/long_message.yaml",
        target_name="simple.yaml",
        options=["--debug", "--time"],
    )
    time_with_debug = time.time() - start_with_debug

    # pad: this used to be 0.1, but got some FPs on unrelated changes like
    # https://github.com/returntocorp/semgrep/runs/6468878138?check_suite_focus=true
    # so I've put 0.2
    assert time_with_debug == pytest.approx(
        time_without_debug, rel=0.2
    ), "adding --debug slowed runtime by more than 20%"
