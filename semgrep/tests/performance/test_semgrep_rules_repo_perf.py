import subprocess
from pathlib import Path

import pytest

# output of `fd ya?ml$`
# not calculating dynamically to avoid slow test collection on each pytest run
RULE_PATHS: str = """
c/goto-fail.yaml
go/antipatterns/channel-guarded-with-mutex.yaml
go/antipatterns/hidden-goroutine.yaml
go/deadcode/useless-eqeq.yaml
go/deadcode/useless-ifelse.yaml
go/gorilla/handler-assignment-from-multiple-sources.yml
go/gorilla/handler-attribute-read-from-multiple-sources.yml
go/gosec/bad_crypto/use_of_weak_crypto.yaml
go/gosec/bad_crypto/use_of_weak_rsa_key.yaml
go/gosec/bad_imports/insecure_ssh.yaml
go/gosec/bad_imports/math_random.yaml
go/gosec/html-template/formatted-template-string.yaml
go/gosec/html-template/unescaped-data-in-htmlattr.yaml
go/gosec/html-template/unescaped-data-in-js.yaml
go/gosec/html-template/unescaped-data-in-url.yaml
go/gosec/overflow/overflow.yaml
go/gosec/pprof/pprof.yaml
go/gosec/socket/bind_all.yaml
go/gosec/sql/string-formatted-query.yaml
go/gosec/tls_ssl_blacklist/ssl.yaml
go/gosec/tls_ssl_blacklist/tls.yaml
go/jwt/jwt.yaml
go/net/use-tls.yaml
go/net/wip-xss-using-responsewriter-and-printf.yaml
java/basic-rules.yaml
java/hardcoded-conditional.yaml
java/security/audit/command-injection-formatted-runtime-call.yaml
java/security/audit/insecure-hostname-verifier.yaml
java/security/audit/insecure-trust-manager.yaml
java/security/audit/spring-cookie-missing-httponly.yaml
java/security/audit/spring-cookie-missing-secure-flag.yaml
java/security/audit/spring-csrf-disabled.yaml
java/security/crypto/no-null-cipher.yaml
java/security/crypto/no-static-initialization-vector.yaml
javascript/deadcode/useless-assign.yaml
javascript/deadcode/useless-eqeq.yaml
javascript/hardcoded-passport-secret/passport-hardcode.yaml
javascript/jwt/audit/jwt-decode-without-verify.yaml
javascript/jwt/jwt-exposed-credentials.yaml
javascript/jwt/jwt-hardcode/jwt-hardcode.yaml
javascript/jwt-none-alg/jwt-none-alg.yaml
javascript/smells/assigned-undefined.yaml
javascript/smells/leftover_debugging.yaml
javascript/spawn-git-clone/spawn-git-clone.yaml
python/airflow/formatted-string-bashoperator.yaml
python/bokeh/deprecated_apis.yaml
python/boto3/security/hardcoded-token.yaml
python/certificates/disabled-cert-validation.yaml
python/click/echo-style.yaml
python/compatibility/python36.yaml
python/compatibility/python37.yaml
python/deadcode/baseclass-attribute-override.yaml
python/deadcode/missing-fstring.yaml
python/deadcode/return.yaml
python/deadcode/useless-assign-keyed.yaml
python/deadcode/useless-assign.notyaml
python/deadcode/useless-comparison.yaml
python/deadcode/useless-eqeq.yaml
python/deadcode/useless-ifelse.yaml
python/deadcode/useless-innerfunction.yaml
python/deadcode/useless-literal.yaml
python/django/best_practices/upsell_django_environ.yaml
python/django/db/model-save.yaml
python/django/db/upsell_count.yaml
python/django/django-2_0-compat.yaml
python/django/json_response/json_response.yaml
python/django/nontext-field-must-set-null-true.yml
python/django/performance/upsell_earliest_latest.yaml
python/django/performance-improvements.yaml
python/django/security/audit/csrf-exempt.yaml
python/django/security/audit/custom-expression-as-sql.yaml
python/django/security/audit/extends-custom-expression.yaml
python/django/security/audit/query-set-extra.yaml
python/django/security/audit/raw-query.yaml
python/django/security/audit/secure-cookies.yml
python/django/security/globals-misuse-code-execution.yaml
python/django/security/injection/command-injection-os-system.yaml
python/django/security/injection/mass-assignment.yaml
python/django/security/injection/open-redirect.yml
python/django/security/injection/path-traversal-file-name.yaml
python/django/security/injection/path-traversal-join.yaml
python/django/security/injection/path-traversal-open.yaml
python/django/security/injection/reflected-data-httpresponse.yaml
python/django/security/injection/reflected-data-httpresponsebadrequest.yaml
python/django/security/injection/request-data-fileresponse.yaml
python/django/security/injection/request-data-write.yaml
python/django/security/injection/sql/sql-injection-extra.yaml
python/django/security/injection/sql/sql-injection-rawsql.yaml
python/django/security/injection/sql/sql-injection-using-db-cursor-execute.yaml
python/django/security/injection/sql/sql-injection-using-raw.yaml
python/django/security/injection/ssrf-injection-requests.yaml
python/django/security/injection/ssrf-injection-urllib.yaml
python/django/security/injection/user-eval-format-string.yaml
python/django/security/injection/user-eval.yaml
python/django/security/injection/user-exec-format-string.yaml
python/django/security/injection/user-exec.yaml
python/django/security/injection/xss-html-email-body.yaml
python/django/security/injection/xss-send-mail-html-message.yaml
python/django/security/passwords/password-empty-string.yml
python/django/security/passwords/unvalidated-password.yml
python/django/security/passwords/use-none-for-password-default.yml
python/django/string-field-null-checks.yml
python/django/use-decimalfield-for-money.yml
python/django/use-onetoonefield.yml
python/exceptions/exceptions.yaml
python/flask/access-request-in-wrong-handler.yaml
python/flask/app-run-param-config/app-run-param-config.yaml
python/flask/app-run-security-config/app-run-security-config.yaml
python/flask/deprecated-apis.yaml
python/flask/different-route-names.yaml
python/flask/hardcoded-config.yaml
python/flask/open-redirect/open-redirect.yml
python/flask/render-template-string/render-template-string.yml
python/flask/same-handler-name.yaml
python/flask/secure-cookies/secure-cookies.yml
python/flask/secure-static-file-serve.yaml
python/flask/security/audit/secure-set-cookie.yaml
python/flask/unsanitized_input.yaml
python/flask/use-jsonify/use-jsonify.yaml
python/logging/listeneval.yaml
python/marshal.yaml
python/multiprocessing/conn_recv.yaml
python/requests/best-practice/use-timeout.yaml
python/requests/security/no-auth-over-http.yaml
python/smells/dict-modify-iterating.yaml
python/smells/exit.yaml
python/smells/list-modify-iterating.yaml
python/smells/manual-collections-create.yaml
python/smells/missing-hash-with-eq.yaml
python/smells/open-never-closed.yml
python/smells/pass-body.yaml
python/smells/return-in-init.yaml
python/smells/sleep.yaml
python/smells/unchecked-returns.yaml
python/socket/bind.yaml
python/sqlalchemy/delete-where.yaml
python/sqlalchemy/performance-improvements.yaml
python/tempfile/flush.yaml
python/tempfile/mktemp.yaml
python/wtf-python/default-mutable-dict.yaml
python/wtf-python/default-mutable-list.yaml
python/wtf-python/is-comparison-string.yml
python/wtf-python/is-not-is-not.yml
"""


@pytest.fixture(scope="session")
def semgrep_rules_repo(tmp_path_factory):
    repo_path = tmp_path_factory.mktemp("repo")
    subprocess.check_output(
        ["git", "clone", "https://github.com/returntocorp/semgrep-rules", repo_path]
    )
    subprocess.check_output(
        ["git", "--git-dir", repo_path / ".git", "checkout", "fe7fb8f7"]  # May 6, 2020
    )

    yield repo_path


@pytest.fixture(params=RULE_PATHS.strip().splitlines())
def semgrep_rules_rule(semgrep_rules_repo, request, tmp_path):
    rule_path = semgrep_rules_repo / Path(request.param)

    # we gather the rule and any non-rule files, which each might be unit tests
    needed_paths = [
        child
        for child in rule_path.parent.iterdir()
        if child.suffix not in {".yml", ".yaml"} or child.name == rule_path.name
    ]
    for needed_path in needed_paths:
        (tmp_path / needed_path.name).symlink_to(needed_path.resolve())
    yield tmp_path


def test_semgrep_rules_rule(semgrep_rules_rule, benchmark):
    benchmark(
        subprocess.check_output,
        [
            "python3",
            "-m",
            "semgrep",
            "--jobs",
            "1",
            "--dangerously-allow-arbitrary-code-execution-from-rules",
            "--strict",
            "--test",
            "--test-ignore-todo",
            semgrep_rules_rule,
        ],
    )
