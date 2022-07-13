import json
from typing import Any
from typing import Mapping

import pytest
from tests.conftest import _clean_output_lsp

from semgrep.lsp.server import SemgrepLSPServer
from semgrep.lsp.types import TextDocumentItem


def mock_text_document_item(source_path, language) -> TextDocumentItem:
    uri = f"file://{source_path}"
    version = 1
    # open and read source_path into var 'text'
    with open(source_path) as f:
        text = f.read()
    while True:
        yield {
            "uri": uri,
            "version": version,
            "text": text,
        }
        version += 1


def mock_workspace_folder(path, name=None):
    return {"name": name if name is not None else f"{path}", "uri": f"file://{path}"}


def init_lsp(lsp, tmp_path, rule_path=None, watch_workspace=False) -> SemgrepLSPServer:
    initialization_options: Mapping[str, Any] = {
        "scan": {},
        "lsp": {
            "watchWorkspace": watch_workspace,
        },
    }
    if rule_path is not None:
        initialization_options["scan"]["configuration"] = [rule_path]
    lsp.m_initialize(
        processId="1",
        rootUri=tmp_path,
        initializationOptions=initialization_options,
        workspaceFolders=[{"name": "tmp_path", "uri": f"file://{tmp_path}"}],
    )
    lsp.m_initialized()


def test_lsp_init(lsp, tmp_path, snapshot):
    init_lsp(lsp, tmp_path, "rules/eqeq-python.yaml")
    assert lsp._ready
    snapshot.assert_match(json.dumps(lsp.config.settings), "settings.json")


def test_lsp_diagnostics(lsp, tmp_path, snapshot):
    init_lsp(lsp, tmp_path, "rules/eqeq-python.yaml")
    lsp.m_text_document__did_open(
        textDocument=next(
            mock_text_document_item(tmp_path / "targets/basic/stupid.py", "python")
        )
    )
    diagonstics = _clean_output_lsp(lsp._diagnostics)
    snapshot.assert_match(diagonstics, "diagnostics.json")


def test_lsp_code_action(lsp, tmp_path, snapshot):
    init_lsp(lsp, tmp_path, "rules/autofix/autofix.yaml")
    # Open document
    lsp.m_text_document__did_open(
        textDocument=next(
            mock_text_document_item(tmp_path / "targets/autofix/autofix.py", "python")
        )
    )
    action_list = lsp.m_text_document__code_action(
        textDocument=next(
            mock_text_document_item(tmp_path / "targets/autofix/autofix.py", "python")
        ),
        range={
            "start": {"line": 0, "character": 0},
            "end": {"line": 1000, "character": 1000},
        },
        context={},
    )
    actions = _clean_output_lsp(action_list)
    snapshot.assert_match(actions, "fixes.json")


def test_lsp_inlay_hint(lsp, tmp_path, snapshot):
    init_lsp(lsp, tmp_path, "rules/eqeq-python.yaml")
    lsp.m_text_document__did_open(
        textDocument=next(
            mock_text_document_item(tmp_path / "targets/basic/stupid.py", "python")
        )
    )

    inlay_hints = lsp.m_text_document__inlay_hint(
        textDocument=next(
            mock_text_document_item(tmp_path / "targets/basic/stupid.py", "python")
        ),
        range={
            "start": {"line": 0, "character": 0},
            "end": {"line": 1000, "character": 1000},
        },
    )

    output = _clean_output_lsp(inlay_hints)
    snapshot.assert_match(output, "inlay_hints.json")


def test_lsp_workspace_folders(lsp, tmp_path):
    init_lsp(lsp, tmp_path, watch_workspace=True)
    dirs = []
    for i in range(10):
        path = tmp_path / f"folder{i}"
        path.mkdir()
        dirs.append(mock_workspace_folder(path))

    event = {
        "added": dirs,
        "removed": [],
    }
    lsp.m_workspace__did_change_workspace_folders(event)
    # add in initial workspace folder of tmp_path
    dirs.insert(0, mock_workspace_folder(tmp_path, "tmp_path"))
    assert lsp.config._workspace_folders == dirs
    event = {
        "added": [],
        "removed": dirs[5:],
    }
    lsp.m_workspace__did_change_workspace_folders(event)
    assert lsp.config._workspace_folders == dirs[:5]


@pytest.mark.kinda_slow
def test_lsp_workspace_auto_config(lsp, tmp_path):
    with open(tmp_path / "semgrep.yaml", "w") as s:
        with open(tmp_path / "rules/eqeq-python.yaml") as f:
            s.write(f.read())
    init_lsp(lsp, tmp_path)
    assert lsp.config.configs == [str(tmp_path / "semgrep.yaml")]


def test_lsp_metrics_measurement(lsp, tmp_path, mocker):
    init_lsp(lsp, tmp_path, "rules/eqeq-python.yaml")
    tmp_file = tmp_path / "foo.py"
    tmp_file.touch()
    with open(tmp_path / "targets/basic/stupid.py") as f:
        with open(tmp_file, "w") as b:
            b.write(f.read())
    lsp.m_text_document__did_open(
        textDocument=next(mock_text_document_item(tmp_file, "python"))
    )
    with open(tmp_file, "w") as b:
        b.write("")
    lsp.m_text_document__did_save(
        textDocument=next(mock_text_document_item(tmp_file, "python"))
    )

    mock_post = mocker.patch("requests.post")
    lsp.m_shutdown()

    payload = json.loads(mock_post.call_args.kwargs["data"])
    assert payload["fix_rate"]["lowerLimits"]["rules.eqeq-is-bad"] == 1
    assert payload["fix_rate"]["upperLimits"]["rules.eqeq-is-bad"] == 1
