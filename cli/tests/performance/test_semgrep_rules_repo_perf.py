import subprocess
from pathlib import Path

import pytest

# output of `fd ya?ml$`
# not calculating dynamically to avoid slow test collection on each pytest run
RULE_PATHS: str = """
c/lang/correctness/goto-fail.yaml
contrib/nodejsscan/archive_path_overwrite.yaml
contrib/nodejsscan/crypto_node.yaml
contrib/nodejsscan/directory_traversal.yaml
contrib/nodejsscan/error_disclosure.yaml
contrib/nodejsscan/eval_deserialize.yaml
contrib/nodejsscan/eval_node.yaml
contrib/nodejsscan/eval_yaml_deserialize.yaml
contrib/nodejsscan/exec_os_command.yaml
contrib/nodejsscan/express_bodyparser_dos.yaml
contrib/nodejsscan/hardcoded_jwt.yaml
contrib/nodejsscan/hardcoded_secrets.yaml
contrib/nodejsscan/header_cors_star.yaml
contrib/nodejsscan/header_helmet_disabled.yaml
contrib/nodejsscan/header_injection.yaml
contrib/nodejsscan/header_xss_protection.yaml
contrib/nodejsscan/jwt_none_algorithm.yaml
contrib/nodejsscan/logic_bypass.yaml
contrib/nodejsscan/nosql_injection.yaml
contrib/nodejsscan/open_redirect.yaml
contrib/nodejsscan/regex_injection.yaml
contrib/nodejsscan/security_electronjs.yaml
contrib/nodejsscan/server_side_template_injection.yaml
contrib/nodejsscan/sql_injection.yaml
contrib/nodejsscan/ssrf_node.yaml
contrib/nodejsscan/timing_attack_node.yaml
contrib/nodejsscan/tls_node.yaml
contrib/nodejsscan/xml_entity_expansion_dos.yaml
contrib/nodejsscan/xpathi_node.yaml
contrib/nodejsscan/xss_node.yaml
contrib/nodejsscan/xss_templates.yaml
contrib/nodejsscan/xxe_node.yaml
go/gorilla/security/audit/handler-assignment-from-multiple-sources.yaml
go/gorilla/security/audit/handler-attribute-read-from-multiple-sources.yaml
go/jwt-go/security/jwt.yaml
go/lang/best-practice/channel-guarded-with-mutex.yaml
go/lang/best-practice/hidden-goroutine.yaml
go/lang/correctness/bad_temp_file/bad_tmp.yaml
go/lang/correctness/dos/decompression_bomb.yaml
go/lang/correctness/overflow/overflow.yaml
go/lang/correctness/permissions/file_permission.yaml
go/lang/correctness/traversal/zip.yaml
go/lang/correctness/unsafe.yaml
go/lang/correctness/useless-eqeq.yaml
go/lang/maintainability/useless-ifelse.yaml
go/lang/security/audit/crypto/bad_imports.yaml
go/lang/security/audit/crypto/insecure_ssh.yaml
go/lang/security/audit/crypto/math_random.yaml
go/lang/security/audit/crypto/ssl.yaml
go/lang/security/audit/crypto/tls.yaml
go/lang/security/audit/crypto/use_of_weak_crypto.yaml
go/lang/security/audit/crypto/use_of_weak_rsa_key.yaml
go/lang/security/audit/database/string-formatted-query.yaml
go/lang/security/audit/net/bind_all.yaml
go/lang/security/audit/net/formatted-template-string.yaml
go/lang/security/audit/net/pprof.yaml
go/lang/security/audit/net/unescaped-data-in-htmlattr.yaml
go/lang/security/audit/net/unescaped-data-in-js.yaml
go/lang/security/audit/net/unescaped-data-in-url.yaml
go/lang/security/audit/net/use-tls.yaml
go/lang/security/audit/net/wip-xss-using-responsewriter-and-printf.yaml
java/lang/correctness/assignment-comparison.yaml
java/lang/correctness/eqeq.yaml
java/lang/correctness/hardcoded-conditional.yaml
java/lang/security/audit/anonymous-ldap-bind.yaml
java/lang/security/audit/bad-hexa-conversion.yaml
java/lang/security/audit/command-injection-formatted-runtime-call.yaml
java/lang/security/audit/crlf-injection-logs.yaml
java/lang/security/audit/crypto/desede-is-deprecated.yaml
java/lang/security/audit/crypto/ecb-cipher.yaml
java/lang/security/audit/crypto/no-null-cipher.yaml
java/lang/security/audit/crypto/no-static-initialization-vector.yaml
java/lang/security/audit/crypto/rsa-no-padding.yaml
java/lang/security/audit/crypto/ssl/avoid-implementing-custom-digests.yaml
java/lang/security/audit/crypto/ssl/defaulthttpclient-is-deprecated.yaml
java/lang/security/audit/crypto/ssl/insecure-hostname-verifier.yaml
java/lang/security/audit/crypto/ssl/insecure-trust-manager.yaml
java/lang/security/audit/crypto/unencrypted-socket.yaml
java/lang/security/audit/crypto/weak-hash.yaml
java/lang/security/audit/crypto/weak-rsa.yaml
java/lang/security/audit/el-injection.yaml
java/lang/security/audit/formatted-sql-string.yaml
java/lang/security/audit/jdbc-sql-formatted-string.yaml
java/lang/security/audit/ldap-entry-poisoning.yaml
java/lang/security/audit/ldap-injection.yaml
java/lang/security/audit/script-engine-injection.yaml
java/lang/security/audit/unvalidated-redirect.yaml
java/lang/security/audit/url-rewriting.yaml
java/lang/security/audit/weak-ssl-context.yaml
java/lang/security/audit/xml-decoder.yaml
java/lang/security/servletresponse-writer-xss.yaml
java/spring/security/audit/spel-injection.yaml
java/spring/security/audit/spring-cookie-missing-httponly.yaml
java/spring/security/audit/spring-cookie-missing-secure-flag.yaml
java/spring/security/audit/spring-csrf-disabled.yaml
java/spring/security/audit/spring-unvalidated-redirect.yaml
javascript/jose/security/jwt-exposed-credentials.yaml
javascript/jose/security/jwt-hardcode.yaml
javascript/jose/security/jwt-none-alg.yaml
javascript/jsonwebtoken/security/audit/jwt-decode-without-verify.yaml
javascript/jsonwebtoken/security/jwt-exposed-credentials.yaml
javascript/jsonwebtoken/security/jwt-hardcode.yaml
javascript/jsonwebtoken/security/jwt-none-alg.yaml
javascript/lang/best-practice/assigned-undefined.yaml
javascript/lang/best-practice/leftover_debugging.yaml
javascript/lang/correctness/useless-assign.yaml
javascript/lang/correctness/useless-eqeq.yaml
javascript/lang/security/path-traversal/path-join-resolve-traversal.yaml
javascript/lang/security/spawn-git-clone.yaml
javascript/passport-jwt/security/passport-hardcode.yaml
python/airflow/security/audit/formatted-string-bashoperator.yaml
python/bokeh/maintainability/deprecated/deprecated_apis.yaml
python/boto3/security/hardcoded-token.yaml
python/click/best-practice/echo-style.yaml
python/django/best-practice/json_response.yaml
python/django/best-practice/upsell_django_environ.yaml
python/django/best-practice/use-onetoonefield.yaml
python/django/compatibility/django-2_0-compat.yaml
python/django/correctness/model-save.yaml
python/django/correctness/nontext-field-must-set-null-true.yaml
python/django/correctness/string-field-null-checks.yaml
python/django/correctness/use-decimalfield-for-money.yaml
python/django/performance/access-foreign-keys.yaml
python/django/performance/upsell-count.yaml
python/django/performance/upsell_earliest_latest.yaml
python/django/security/audit/csrf-exempt.yaml
python/django/security/audit/custom-expression-as-sql.yaml
python/django/security/audit/extends-custom-expression.yaml
python/django/security/audit/query-set-extra.yaml
python/django/security/audit/raw-query.yaml
python/django/security/audit/secure-cookies.yaml
python/django/security/audit/unvalidated-password.yaml
python/django/security/injection/code/globals-misuse-code-execution.yaml
python/django/security/injection/code/user-eval-format-string.yaml
python/django/security/injection/code/user-eval.yaml
python/django/security/injection/code/user-exec-format-string.yaml
python/django/security/injection/code/user-exec.yaml
python/django/security/injection/command/command-injection-os-system.yaml
python/django/security/injection/email/xss-html-email-body.yaml
python/django/security/injection/email/xss-send-mail-html-message.yaml
python/django/security/injection/mass-assignment.yaml
python/django/security/injection/open-redirect.yaml
python/django/security/injection/path-traversal/path-traversal-file-name.yaml
python/django/security/injection/path-traversal/path-traversal-join.yaml
python/django/security/injection/path-traversal/path-traversal-open.yaml
python/django/security/injection/reflected-data-httpresponse.yaml
python/django/security/injection/reflected-data-httpresponsebadrequest.yaml
python/django/security/injection/request-data-fileresponse.yaml
python/django/security/injection/request-data-write.yaml
python/django/security/injection/sql/sql-injection-extra.yaml
python/django/security/injection/sql/sql-injection-rawsql.yaml
python/django/security/injection/sql/sql-injection-using-db-cursor-execute.yaml
python/django/security/injection/sql/sql-injection-using-raw.yaml
python/django/security/injection/ssrf/ssrf-injection-requests.yaml
python/django/security/injection/ssrf/ssrf-injection-urllib.yaml
python/django/security/passwords/password-empty-string.yaml
python/django/security/passwords/use-none-for-password-default.yaml
python/flask/best-practice/use-jsonify.yaml
python/flask/correctness/access-request-in-wrong-handler.yaml
python/flask/correctness/same-handler-name.yaml
python/flask/experimental/correctness/different-route-names.yaml
python/flask/experimental/security/audit/secure-cookies/secure-session-cookies.yaml
python/flask/maintainability/deprecated/deprecated-apis.yaml
python/flask/security/audit/app-run-param-config.yaml
python/flask/security/audit/app-run-security-config.yaml
python/flask/security/audit/hardcoded-config.yaml
python/flask/security/audit/secure-set-cookie.yaml
python/flask/security/audit/wtf-csrf-disabled.yaml
python/flask/security/open-redirect.yaml
python/flask/security/render-template-string.yaml
python/flask/security/secure-static-file-serve.yaml
python/flask/security/unescaped-template-extension.yaml
python/flask/security/unsanitized_input.yaml
python/lang/best-practice/manual-collections-create.yaml
python/lang/best-practice/missing-hash-with-eq.yaml
python/lang/best-practice/open-never-closed.yaml
python/lang/best-practice/pass-body.yaml
python/lang/best-practice/sleep.yaml
python/lang/compatibility/python36.yaml
python/lang/compatibility/python37.yaml
python/lang/correctness/baseclass-attribute-override.yaml
python/lang/correctness/common-mistakes/default-mutable-dict.yaml
python/lang/correctness/common-mistakes/default-mutable-list.yaml
python/lang/correctness/common-mistakes/is-comparison-string.yaml
python/lang/correctness/common-mistakes/is-not-is-not.yaml
python/lang/correctness/dict-modify-iterating.yaml
python/lang/correctness/exceptions/exceptions.yaml
python/lang/correctness/exit.yaml
python/lang/correctness/list-modify-iterating.yaml
python/lang/correctness/missing-fstring.yaml
python/lang/correctness/return-in-init.yaml
python/lang/correctness/tempfile/flush.yaml
python/lang/correctness/tempfile/mktemp.yaml
python/lang/correctness/unchecked-returns.yaml
python/lang/correctness/useless-comparison.yaml
python/lang/correctness/useless-eqeq.yaml
python/lang/maintainability/return.yaml
python/lang/maintainability/useless-assign-keyed.yaml
python/lang/maintainability/useless-assign.notyaml
python/lang/maintainability/useless-ifelse.yaml
python/lang/maintainability/useless-innerfunction.yaml
python/lang/maintainability/useless-literal.yaml
python/lang/security/audit/conn_recv.yaml
python/lang/security/audit/logging/listeneval.yaml
python/lang/security/audit/marshal.yaml
python/lang/security/audit/network/bind.yaml
python/lang/security/audit/network/disabled-cert-validation.yaml
python/requests/best-practice/use-timeout.yaml
python/requests/security/no-auth-over-http.yaml
python/sqlalchemy/correctness/delete-where.yaml
python/sqlalchemy/performance/performance-improvements.yaml
"""


@pytest.fixture(scope="session")
def semgrep_rules_repo(request, tmp_path_factory):
    repo_path = tmp_path_factory.mktemp("repo")

    getoption = request.config.getoption
    is_disabled = getoption("benchmark_skip") or getoption("benchmark_disable")
    is_force_enabled = getoption("benchmark_enable") or getoption("benchmark_only")
    if is_disabled and not is_force_enabled:
        return repo_path  # not cloning since we're not gonna run benchmarks

    subprocess.check_output(
        ["git", "clone", "https://github.com/returntocorp/semgrep-rules", repo_path]
    )
    subprocess.check_output(
        ["git", "checkout", "c3196b4"], cwd=repo_path  # May 16, 2020
    )

    return repo_path


@pytest.fixture(params=RULE_PATHS.strip().splitlines())
def semgrep_rules_rule(semgrep_rules_repo, request, tmp_path, benchmark):
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
