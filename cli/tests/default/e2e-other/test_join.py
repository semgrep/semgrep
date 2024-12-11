import pytest
from tests.fixtures import RunSemgrep


@pytest.mark.kinda_slow
@pytest.mark.parametrize(
    "rule,target",
    [
        (
            "rules/join_rules/user-input-escaped-with-safe.yaml",
            "join_rules/user-input-escaped-with-safe",
        ),
        (
            "rules/join_rules/user-input-with-unescaped-extension.yaml",
            "join_rules/user-input-with-unescaped-extension",
        ),
        (
            "rules/join_rules/multiple-rules.yaml",
            "join_rules/user-input-with-unescaped-extension",
        ),
        (
            "rules/join_rules/inline/inline-rules.yaml",
            "join_rules/user-input-with-unescaped-extension",
        ),
        (
            "rules/join_rules/inline/taint.yaml",
            "join_rules/user-input-with-unescaped-extension",
        ),
    ],
)
@pytest.mark.osemfail
def test_basic(run_semgrep_in_tmp: RunSemgrep, snapshot, rule, target):
    snapshot.assert_match(
        run_semgrep_in_tmp(
            rule,
            target_name=target,
            # we now need to be logged in to access metavariables
            # which are needed for join mode to work
            is_logged_in_weak=True,
        ).stdout,
        "results.json",
    )


@pytest.mark.kinda_slow
@pytest.mark.parametrize(
    "rule,target",
    [
        (
            "rules/join_rules/recursive/java-callgraph-example/vulnado-sqli.yaml",
            "join_rules/recursive/java-callgraph-example/vulnado",
        ),
        (
            "rules/join_rules/recursive/java-callgraph-example/vulnado-sqli.yaml",
            "join_rules/recursive/java-callgraph-example/vulnado-chain-broken",
        ),
        # TODO: regression because of
        # https://github.com/returntocorp/semgrep/pull/6900
        # commented for now
        #
        # Let's say you have a pattern `func($X)` to match `func(bar)` where `bar`
        # comes from package `foo`. Semgrep-core returns two matches, in each match
        # the abstract_content of $X is different, in one it is `bar` and in the
        # other it is `foo.bar`. Right, but the token associated with both is now
        # the same since PR #6900, it's the token of `bar` for both matches! And
        # CLI ignores abstract_content and instead takes that token and reads the
        # content from the file, so for CLI both matches are exactly the same, they
        # set $X to `bar`. So it picks any one of them. However, join-mode does use
        # abstract_content, so the difference between the matches does matter. Then,
        # if join-mode tries to join rules R and S, and for rule R the CLI picked a
        # match with abstract_content equals to `foo.bar`, whereas for rule S the
        # CLI picked a match with abstract_content equals to `bar`... it won't work.
        # (
        #     "rules/join_rules/recursive/flask-deep-stored-xss-example/flask-stored-xss.yaml",
        #     "join_rules/recursive/flask-deep-stored-xss-example",
        # ),
    ],
)
@pytest.mark.osemfail
def test_recursive(run_semgrep_in_tmp: RunSemgrep, snapshot, rule, target):
    snapshot.assert_match(
        run_semgrep_in_tmp(rule, target_name=target, is_logged_in_weak=True).stdout,
        "results.json",
    )
