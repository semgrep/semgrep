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
import shutil
import subprocess
import tempfile
import time
import uuid
from pathlib import Path
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


@pytest.fixture
def mock_workspaces(mock_files, tmp_path):
    workspace1 = mock_files
    # Copy mock files to a second workspace
    # This is gross IK but oh well
    workspace2_root = str(tmp_path)[:-1] + "1"
    shutil.copytree(workspace1[0], workspace2_root)
    workspace2_paths = [
        Path(workspace2_root) / (Path(file)).name for file in workspace1[1]
    ]
    workspace2_files = sorted(str(file) for file in workspace2_paths)

    workspace2 = workspace2_root, workspace2_files
    return [workspace1, workspace2]


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
    if len(workspace_folders) > 0:
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


def send_did_add(server, path):
    send_msg(
        server,
        "workspace/didCreateFiles",
        {
            "files": [
                {
                    "uri": f"file://{path}",
                }
            ]
        },
        notif=True,
    )


def send_did_delete(server, path):
    send_msg(
        server,
        "workspace/didDeleteFiles",
        {
            "files": [
                {
                    "uri": f"file://{path}",
                }
            ]
        },
        notif=True,
    )


def send_did_change_folder(server, added=None, removed=None):
    if added is None:
        added = []
    if removed is None:
        removed = []
    addded_json = []
    for file in added:
        addded_json.append(
            {
                "uri": f"file://{file}",
                "name": f"file://{file}",
            }
        )
    removed_json = []
    for file in removed:
        removed_json.append(
            {
                "uri": f"file://{file}",
                "name": f"file://{file}",
            }
        )
    send_msg(
        server,
        "workspace/didChangeWorkspaceFolders",
        {
            "event": {
                "added": addded_json,
                "removed": removed_json,
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


def send_semgrep_search(server, pattern, language=None):
    params = {"pattern": pattern}
    if language:
        params["language"] = language
    send_msg(
        server,
        "semgrep/search",
        params,
    )


def check_diagnostics(response, file, expected_ids):
    assert response["method"] == "textDocument/publishDiagnostics"
    assert response["params"]["uri"] == f"file://{file}"
    ids = [d["code"] for d in response["params"]["diagnostics"]]
    assert ids == expected_ids


def assert_notif(response, method, message=None, kind=None):
    assert response["method"] == method
    if message:
        assert response["params"]["value"]["message"] == message
    if kind:
        assert response["params"]["value"]["kind"] == kind


def assert_progress(responses, message):
    response = next(responses)
    assert_notif(response, "window/workDoneProgress/create")

    response = next(responses)
    assert_notif(response, "$/progress", message)

    response = next(responses)
    assert_notif(response, "$/progress", kind="end")


# Goes through initial startup cycle, and checks for correct responses
def check_startup(server, responses, folders, logged_in, files):
    # initialize
    send_initialize(server, folders)

    if len(folders) > 1:
        # Multi-workspace May degrade performance message
        response = next(responses)
        assert_notif(
            response,
            "window/showMessage",
        )

    response = next(responses)
    assert "capabilities" in response["result"]

    send_initialized(server)
    assert_progress(responses, "Loading Rules")

    assert_progress(responses, "Scanning Workspace")

    scanned_files = list(filter(lambda x: "existing" not in x, files))

    scan_responses = [next(responses) for _ in range(len(scanned_files))]

    scan_responses = sorted(scan_responses, key=lambda x: x["params"]["uri"])  # type: ignore

    ran_rule_id = "eqeq-four" if logged_in else "eqeq-five"

    expected_ids = [ran_rule_id]
    for i, file in enumerate(scanned_files):
        response = scan_responses[i]
        check_diagnostics(response, file, expected_ids)


# TODO: this is currently passing with osemgrep but should not. it's because we don't
# call osemgrep. If we were calling osemgrep correctly, we would get failures.
# Note: This also fails in the 'test osemgrep' CI job with some errors about git

# Test anything that is part of the normal LSP specs, and basic usage
@pytest.mark.slow()
def test_ls_specs(
    run_semgrep_ls,  # nosemgrep: typehint-run-semgrep
    mock_files,
):

    root, files = mock_files

    server, responses = run_semgrep_ls

    check_startup(server, responses, [root], False, files)

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

    # Test did add
    added = root / "added.py"
    shutil.copy(files[0], added)
    send_did_add(server, added)
    send_did_open(server, added)

    response = next(responses)
    check_diagnostics(response, added, ["eqeq-five", "eqeq-five"])

    send_did_delete(server, added)

    response = next(responses)
    check_diagnostics(response, added, [])

    send_exit(server)


# Test any extensions to the LSP specs, basically anything custom to semgrep
@pytest.mark.parametrize("logged_in", [True, False])
def test_ls_ext(
    logged_in,
    run_semgrep_ls,  # nosemgrep: typehint-run-semgrep
    mock_files,
    mocker,
    monkeypatch,
):
    server, responses = run_semgrep_ls

    root, files = mock_files

    monkeypatch.chdir(root)

    if logged_in:
        mocker.patch("semgrep.app.auth.get_deployment_from_token", return_value="1")
        mocker.patch("semgrep.app.auth.get_token", return_value="token")
    else:
        mocker.patch("semgrep.app.auth.get_deployment_from_token", return_value=None)

    check_startup(server, responses, [root], logged_in, files)

    # scan workspace
    send_semgrep_scan_workspace(server)
    assert_progress(responses, "Scanning Workspace")

    num_ids = []
    scanned_files = list(filter(lambda x: "existing" not in x, files))
    for _ in scanned_files:
        response = next(responses)
        num_ids.append(len(response["params"]["diagnostics"]))

    # scan workspace full
    send_semgrep_scan_workspace_full(server)

    assert_progress(responses, "Scanning Workspace")

    for i in range(len(files)):
        response = next(responses)
        uri = response["params"]["uri"]
        if "modified" in uri:
            assert len(response["params"]["diagnostics"]) > num_ids[i]

    ## login
    send_semgrep_login(server)
    response = next(responses)

    if logged_in:
        assert_notif(response, "window/showMessage")
    else:
        assert response["result"]["url"] != ""
        assert response["result"]["sessionId"] != ""

        mocker.patch("semgrep.app.auth.get_deployment_from_token", return_value="1")
        mocker.patch("semgrep.app.auth.get_token", return_value="token")

        send_semgrep_login_finish(server)
        response = next(responses)
        # Waiting for login
        assert_notif(response, "window/showMessage")

        response = next(responses)
        # login success
        assert_notif(response, "window/showMessage")

        # More progress bars
        for _ in range(0, 6):
            next(responses)

        for _ in files:
            response = next(responses)
            uri = response["params"]["uri"]
            ids = [d["code"] for d in response["params"]["diagnostics"]]
            if "existing" not in uri:
                assert "eqeq-four" in ids
                assert "eqeq-five" not in ids
            else:
                assert len(ids) == 0

    # logout
    mocker.patch("semgrep.app.auth.get_deployment_from_token", return_value=None)
    send_semgrep_logout(server)

    # Logged out succesfully
    response = next(responses)
    assert_notif(response, "window/showMessage")

    # More progress bars
    for _ in range(0, 6):
        next(responses)

    for _ in files:
        response = next(responses)
        uri = response["params"]["uri"]
        ids = [d["code"] for d in response["params"]["diagnostics"]]
        if "existing" not in uri:
            assert "eqeq-four" not in ids
            assert "eqeq-five" in ids
        else:
            assert len(ids) == 0

    send_semgrep_search(server, "print(...)")
    response = next(responses)
    results = response["result"]
    assert len(results["locations"]) == 3

    send_exit(server)


# Test functionality of multi-workspaces
@pytest.mark.slow()
def test_ls_multi(run_semgrep_ls, mock_workspaces):  # nosemgrep: typehint-run-semgrep
    workspace1, workspace2 = mock_workspaces
    server, responses = run_semgrep_ls

    workspace_folders = [workspace1[0], workspace2[0]]
    files = workspace1[1] + workspace2[1]
    scanned_files = list(filter(lambda x: "existing" not in x, files))

    check_startup(server, responses, workspace_folders, False, files)

    send_did_change_folder(server, removed=[workspace1[0]])

    assert_progress(responses, "Scanning Workspace")

    for file in scanned_files:
        response = next(responses)
        # If it's a workspace we removed, then we expect no diagnostics
        if str(workspace1[0]) in file:
            assert len(response["params"]["diagnostics"]) == 0
        else:
            check_diagnostics(response, file, ["eqeq-five"])

    send_did_change_folder(server, added=[workspace1[0]])

    assert_progress(responses, "Scanning Workspace")

    for file in scanned_files:
        check_diagnostics(next(responses), file, ["eqeq-five"])

    send_exit(server)


@pytest.mark.slow()
def test_ls_no_folders(run_semgrep_ls):  # nosemgrep: typehint-run-semgrep
    server, responses = run_semgrep_ls
    check_startup(server, responses, [], False, [])

    send_exit(server)
