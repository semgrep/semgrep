def test_eqeq(run_semgrep_in_tmp, benchmark):
    benchmark(run_semgrep_in_tmp, "rules/eqeq.yaml", options=["--jobs", "1"])
