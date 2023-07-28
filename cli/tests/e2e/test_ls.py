import re
import shutil
import subprocess
import time
import uuid
from pathlib import Path

import pytest

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
            "configuration": [
                str(Path(__file__).parent / "targets" / "ls" / "rules.yaml")
            ],
            "exclude": [],
            "include": [],
            "jobs": 5,
            "maxMemory": 0,
            "maxTargetBytes": 0,
            "onlyGitDirty": True,
            "ci": False,
        },
        "trace": {"server": "verbose"},
        "metrics": {
            "enabled": True,
        },
        "doHover": True,
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

PROG_REGEX = re.compile(r"Pr([\s\S]*)")


@pytest.fixture
def run_semgrep_ls(mocker):
    server = SemgrepCoreLSServer()
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
            call_args = server.std_writer.write.call_args_list[counter]
            print(call_args)
            print(call_args[0])
            print(call_args[0][0])
            yield server.std_writer.write.call_args_list[counter][0][0]
            counter += 1

    return server, response_iterator()


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

    send_msg(server, "exit", notif=True)


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


def send_hover(server, path, position, line):

    params = {
        "textDocument": {"uri": f"file://{path}"},
        "position": {"character": position, "line": line},
        "workDoneToken": "foo",
    }

    send_msg(server, "textDocument/hover", params)


def send_semgrep_show_ast(server, uri, named=False):
    params = {"uri": uri, "named": named}
    send_msg(
        server,
        "semgrep/showAst",
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
def check_startup(server, responses, folders, files):
    # initialize
    send_initialize(server, folders)

    response = next(responses)
    assert "capabilities" in response["result"]

    send_initialized(server)
    assert_progress(responses, "Refreshing Rules")

    assert_progress(responses, "Scanning Workspace")

    scanned_files = list(filter(lambda x: "existing" not in x, files))

    scan_responses = [next(responses) for _ in range(len(scanned_files))]

    scan_responses = sorted(scan_responses, key=lambda x: x["params"]["uri"])  # type: ignore

    expected_ids = ["eqeq-five"]
    for i, file in enumerate(scanned_files):
        response = scan_responses[i]
        check_diagnostics(response, file, expected_ids)


# Test anything that is part of the normal LSP specs, and basic usage
@pytest.mark.slow()
def test_ls_specs(
    run_semgrep_ls,  # nosemgrep: typehint-run-semgrep
    mock_files,
):

    root, files = mock_files

    server, responses = run_semgrep_ls

    check_startup(server, responses, [root], files)

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
@pytest.mark.slow
def test_ls_ext(
    run_semgrep_ls,  # nosemgrep: typehint-run-semgrep
    mock_files,
    monkeypatch,
):
    server, responses = run_semgrep_ls

    root, files = mock_files

    monkeypatch.chdir(root)

    check_startup(server, responses, [root], files)

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
            print(response)
            assert len(response["params"]["diagnostics"]) > num_ids[i]

    send_semgrep_search(server, "print(...)")
    response = next(responses)
    results = response["result"]
    assert len(results["locations"]) == 3

    # hover is on by default
    for file in files:
        send_hover(server, file, position=1, line=0)
        response = next(responses)
        results = response["result"]
        assert results is not None
        # Make sure the contents field exists
        # This test might break actually if there is no hover
        # for the given test file
        results["contents"]

    # showAst
    for file in files:
        send_semgrep_show_ast(server, f"file://{file}")
        response = next(responses)
        results = response["result"]
        # output looks like a program AST
        assert PROG_REGEX.match(results)

    send_exit(server)


# Test functionality of multi-workspaces
@pytest.mark.slow()
def test_ls_multi(run_semgrep_ls, mock_workspaces):  # nosemgrep: typehint-run-semgrep
    workspace1, workspace2 = mock_workspaces
    server, responses = run_semgrep_ls

    workspace_folders = [workspace1[0], workspace2[0]]
    files = workspace1[1] + workspace2[1]
    scanned_files = list(filter(lambda x: "existing" not in x, files))

    check_startup(server, responses, workspace_folders, files)

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
    check_startup(server, responses, [], [])

    send_exit(server)
