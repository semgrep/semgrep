#  e2e tests
#  didOpen/didSave + diagnostics
#  codeAction
#  login/logout process
#  scan workspace (Both)
#  refresh rules
#  git dirty vs no git dirty
#  git vs no git
#  stress tests of: open multiple files, save multiple files, deleting files
#                   scan workspace, refresh rules, login/logout
import json
import subprocess
import tempfile
import time
import uuid
from textwrap import dedent

import pytest
from requests import Response

from semgrep.app.scans import ScanHandler
from semgrep.app.session import AppSession
from semgrep.config_resolver import ConfigLoader
from semgrep.lsp.server import SemgrepCoreLSServer


DEFAULT_CAPABILITIES = {
    "processId": 1234,
    "clientInfo": {"name": "Visual Studio Code", "version": "1.68.1"},
    "locale": "en-us",
    "rootPath": "",
    "rootUri": "",
    "workspaceFolders": [],
    "initializationOptions": {
        "scan": {
            "configuration": [],
            "exclude": [],
            "include": [],
            "jobs": 5,
            "maxMemory": 0,
            "maxTargetBytes": 0,
            "onlyGitDirty": True,
        },
        "trace": {"server": "verbose"},
        "metrics": {
            "enabled": True,
        },
    },
    "capabilities": {},
}

DEFAULT_CONTENT = """
x = 0
if x == 5: # auto rule
    print("hello")
if x == 4: # CI rule
    print("hello")
"""


@pytest.fixture
def run_semgrep_ls(mocker):
    tmp_rule_file = tempfile.NamedTemporaryFile("w+", suffix=".json")
    tmp_target_file = tempfile.NamedTemporaryFile("w+", suffix=".json")

    server = SemgrepCoreLSServer(tmp_rule_file, tmp_target_file)  # type: ignore
    server.std_writer = mocker.Mock()

    def response_iterator():
        counter = 0
        while True:
            try_counter = 0
            while server.std_writer.write.call_count <= counter:
                try_counter += 1
                time.sleep(0.1)
                assert (
                    try_counter < 100
                ), "Timed out waiting for response from Semgrep LS"
            yield server.std_writer.write.call_args_list[counter][0][0]
            counter += 1

    return server, response_iterator()


@pytest.fixture(autouse=True)
def ci_mock(mocker):

    auto_file_content = dedent(
        """
        rules:
        - id: eqeq-five
          pattern: $X == 5
          message: "useless comparison to 5"
          languages: [python]
          severity: ERROR
          metadata:
            dev.semgrep.actions: []
          fix: $X == 2
        """
    ).lstrip()

    ci_file_content = dedent(
        """
{
  "rules": [
    {
      "id": "eqeq-four",
      "pattern": "$X == 4",
      "message": "useless comparison to 4",
      "languages": [
        "python"
      ],
      "severity": "ERROR",
      "metadata": {
        "dev.semgrep.actions": [
          "block"
        ]
      },
      "fix": "$X == 2"
    }
  ]
}        """
    ).lstrip()

    mocker.patch.object(
        ConfigLoader, "_make_config_request", return_value=auto_file_content
    )
    mocker.patch.object(
        ScanHandler,
        "_get_scan_config_from_app",
        return_value={
            "deployment_id": 1,
            "deployment_name": "org_name",
            "ignored_files": [],
            "policy_names": ["audit", "comment", "block"],
            "rule_config": ci_file_content,
        },
    )
    response = Response()
    response.status_code = 200
    response._content = json.dumps({"token": "token"}).encode()
    mocker.patch.object(AppSession, "post", return_value=response)


@pytest.fixture
def mock_files(git_tmp_path):
    root = git_tmp_path

    modified_file = (
        root / "modified.py"
    )  # should have preexisting matches that are committed
    existing_file = (
        root / "existing.py"
    )  # should have preexisting matches that are not committed
    open(modified_file, "w").write(DEFAULT_CONTENT)
    open(existing_file, "w").write(DEFAULT_CONTENT)

    subprocess.run(["git", "remote", "add", "origin", "/tmp/origin"], check=True)
    subprocess.run(["git", "add", "modified.py"], check=True)
    subprocess.run(["git", "add", "existing.py"], check=True)
    subprocess.run(["git", "commit", "-m", "initial commit"], check=True)

    open(modified_file, "a").write(DEFAULT_CONTENT)

    new_file = root / "new.py"  # created after commit
    open(new_file, "w").write(DEFAULT_CONTENT)

    files = [existing_file, modified_file, new_file]
    files = sorted(str(file) for file in files)

    return root, files


def send_msg(server, method, params=None, notif=False):
    msg = {"jsonrpc": "2.0", "method": method}
    if params:
        msg["params"] = params

    if not notif:
        msg["id"] = str(uuid.uuid4())
    server.on_std_message(msg)


def send_exit(server):

    send_msg(server, "exit")


def send_initialize(server, workspace_folders, only_git_dirty=True):
    workspace_folders = [f"file://{folder}" for folder in workspace_folders]
    params = DEFAULT_CAPABILITIES
    params["rootUri"] = workspace_folders[0]
    params["workspaceFolders"] = [
        {"uri": folder, "name": folder} for folder in workspace_folders
    ]
    params["initializationOptions"]["scan"]["onlyGitDirty"] = only_git_dirty  # type: ignore
    send_msg(server, "initialize", params)


def send_initialized(server):
    send_msg(server, "initialized", notif=True)


def send_did_open(server, path):
    send_msg(
        server,
        "textDocument/didOpen",
        {
            "textDocument": {
                "uri": f"file://{path}",
                "languageId": "python",
                "version": 1,
                "text": open(path).read(),
            }
        },
        notif=True,
    )


def send_did_save(server, path):
    send_msg(
        server,
        "textDocument/didSave",
        {
            "textDocument": {
                "uri": f"file://{path}",
                "version": 1,
            }
        },
        notif=True,
    )


def send_code_action(
    server, path, diagnostics, line_start, char_start, line_end, char_end
):
    send_msg(
        server,
        "textDocument/codeAction",
        {
            "textDocument": {
                "uri": f"file://{path}",
            },
            "range": {
                "start": {
                    "line": line_start,
                    "character": char_start,
                },
                "end": {
                    "line": line_end,
                    "character": char_end,
                },
            },
            "context": {
                "diagnostics": diagnostics,
            },
        },
    )


def send_semgrep_login(server):
    send_msg(server, "semgrep/login")


def send_semgrep_login_finish(server):
    send_msg(server, "semgrep/loginFinish", {"url": "", "sessionId": ""}, notif=True)


def send_semgrep_logout(server):
    send_msg(server, "semgrep/logout", notif=True)


def send_semgrep_scan_workspace(server):
    send_msg(server, "semgrep/scanWorkspace", {"full": False}, notif=True)


def send_semgrep_scan_workspace_full(server):
    send_msg(server, "semgrep/scanWorkspace", {"full": True}, notif=True)


def send_semgrep_refresh_rules(server):
    send_msg(server, "semgrep/refreshRules", notif=True)


def send_semgrep_search(server, language, pattern):
    send_msg(
        server,
        "semgrep/search",
        {
            "language": language,
            "pattern": pattern,
        },
    )


def check_diagnostics(response, file, expected_ids):
    assert response["method"] == "textDocument/publishDiagnostics"
    assert response["params"]["uri"] == f"file://{file}"
    ids = [d["code"] for d in response["params"]["diagnostics"]]
    assert ids == expected_ids


# TODO: this is currently passing with osemgrep but should not. it's because we don't
# call osemgrep. If we were calling osemgrep correctly, we would get failures.
# Note: This also fails in the 'test osemgrep' CI job with some errors about git
@pytest.mark.parametrize("logged_in", [True, False])
@pytest.mark.slow()
def test_ls_full(
    logged_in,
    run_semgrep_ls,  # nosemgrep: typehint-run-semgrep
    mock_files,
    mocker,
    monkeypatch,
):

    root, files = mock_files

    monkeypatch.chdir(root)

    if logged_in:
        mocker.patch("semgrep.app.auth.get_deployment_from_token", return_value="1")
        mocker.patch("semgrep.app.auth.get_token", return_value="token")
    else:
        mocker.patch("semgrep.app.auth.get_deployment_from_token", return_value=None)

    server, responses = run_semgrep_ls

    # initialize
    send_initialize(server, [root])

    response = next(responses)
    assert "capabilities" in response["result"]

    send_initialized(server)
    response = next(responses)
    assert response["method"] == "window/workDoneProgress/create"

    response = next(responses)
    assert response["method"] == "$/progress"
    assert response["params"]["value"]["message"] == "Loading Rules"

    response = next(responses)
    assert response["method"] == "$/progress"
    assert response["params"]["value"]["kind"] == "end"

    response = next(responses)
    assert response["method"] == "window/workDoneProgress/create"

    response = next(responses)
    assert response["method"] == "$/progress"
    assert response["params"]["value"]["message"] == "Scanning Workspace"

    response = next(responses)
    assert response["method"] == "$/progress"
    assert response["params"]["value"]["kind"] == "end"

    scanned_files = files[1:]

    scan_responses = [next(responses) for _ in range(len(scanned_files))]

    scan_responses = sorted(scan_responses, key=lambda x: x["params"]["uri"])  # type: ignore

    ran_rule_id = "eqeq-four" if logged_in else "eqeq-five"

    expected_ids = [ran_rule_id]
    response = scan_responses[0]
    check_diagnostics(response, scanned_files[0], expected_ids)

    response = scan_responses[1]
    check_diagnostics(response, scanned_files[1], expected_ids)

    expected_ids = []
    for file in files:
        # didOpen
        send_did_open(server, file)
        # add content
        response = next(responses)
        old_ids = [d["code"] for d in response["params"]["diagnostics"]]
        open(file, "a").write(DEFAULT_CONTENT)

        # didSave
        send_did_save(server, file)
        response = next(responses)
        new_ids = [d["code"] for d in response["params"]["diagnostics"]]
        assert len(new_ids) == len(old_ids) + 1

        diagnostic_context = response["params"]["diagnostics"][-1:]
        # get range of last diagnostic
        line_start = diagnostic_context[0]["range"]["start"]["line"]
        char_start = diagnostic_context[0]["range"]["start"]["character"]
        line_end = diagnostic_context[0]["range"]["end"]["line"]
        char_end = diagnostic_context[0]["range"]["end"]["character"]

        # get code actions
        send_code_action(
            server, file, diagnostic_context, line_start, char_start, line_end, char_end
        )
        response = next(responses)
        assert len(response["result"]) == 1
        assert response["result"][0]["kind"] == "quickfix"

    # scan workspace
    send_semgrep_scan_workspace(server)
    response = next(responses)
    assert response["method"] == "window/workDoneProgress/create"

    response = next(responses)
    assert response["method"] == "$/progress"
    assert response["params"]["value"]["message"] == "Scanning Workspace"

    response = next(responses)
    assert response["method"] == "$/progress"
    assert response["params"]["value"]["kind"] == "end"

    num_ids = []
    for _ in files:
        response = next(responses)
        num_ids.append(len(response["params"]["diagnostics"]))

    # scan workspace full
    send_semgrep_scan_workspace_full(server)

    response = next(responses)
    assert response["method"] == "window/workDoneProgress/create"

    response = next(responses)
    assert response["method"] == "$/progress"
    assert response["params"]["value"]["message"] == "Scanning Workspace"

    response = next(responses)
    assert response["method"] == "$/progress"
    assert response["params"]["value"]["kind"] == "end"

    for i in range(len(files)):
        response = next(responses)
        uri = response["params"]["uri"]
        if "existing" in uri or "modified" in uri:
            assert len(response["params"]["diagnostics"]) > num_ids[i]

    ## login
    send_semgrep_login(server)
    response = next(responses)

    if logged_in:
        assert response["method"] == "window/showMessage"
    else:
        assert response["result"]["url"] != ""
        assert response["result"]["sessionId"] != ""

        mocker.patch("semgrep.app.auth.get_deployment_from_token", return_value="1")
        mocker.patch("semgrep.app.auth.get_token", return_value="token")

        send_semgrep_login_finish(server)
        response = next(responses)
        # Waiting for login
        assert response["method"] == "window/showMessage"

        response = next(responses)
        # login success
        assert response["method"] == "window/showMessage"

        # More progress bars
        for _ in range(0, 6):
            next(responses)

        for _ in files:
            response = next(responses)
            uri = response["params"]["uri"]
            ids = [d["code"] for d in response["params"]["diagnostics"]]
            assert "eqeq-four" in ids
            assert "eqeq-five" not in ids

    # logout
    mocker.patch("semgrep.app.auth.get_deployment_from_token", return_value=None)
    send_semgrep_logout(server)

    # Logged out succesfully
    response = next(responses)
    assert response["method"] == "window/showMessage"

    # More progress bars
    for _ in range(0, 6):
        next(responses)

    for _ in files:
        response = next(responses)
        uri = response["params"]["uri"]
        ids = [d["code"] for d in response["params"]["diagnostics"]]
        assert "eqeq-five" in ids
        assert "eqeq-four" not in ids

    send_semgrep_search(server, "python", "print(...)")
    response = next(responses)
    results = response["result"]
    assert len(results["locations"]) == 3
