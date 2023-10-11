import builtins
from io import StringIO
from pathlib import Path
from textwrap import dedent

import pytest
from ruamel.yaml import YAML

import semgrep.semgrep_interfaces.semgrep_output_v1 as out
from semgrep.formatter.sarif import SarifFormatter
from semgrep.rule import Rule
from semgrep.rule_lang import EmptySpan
from semgrep.rule_lang import YamlTree
from semgrep.rule_match import RuleMatch

yaml = YAML(typ="rt")


def create_taint_rule_match():
    match = RuleMatch(
        message="message",
        severity=out.MatchSeverity(out.Error()),
        match=out.CoreMatch(
            check_id=out.RuleId("rule.id"),
            path=out.Fpath("foo.py"),
            start=out.Position(3, 4, 6),
            end=out.Position(3, 5, 7),
            extra=out.CoreMatchExtra(
                metavars=out.Metavars({}),
                dataflow_trace=out.MatchDataflowTrace(
                    taint_source=out.MatchCallTrace(
                        out.CliLoc(
                            (
                                out.Location(
                                    path=out.Fpath("foo.py"),
                                    start=out.Position(8, 9, 11),
                                    end=out.Position(8, 10, 12),
                                ),
                                "??",
                            )
                        )
                    ),
                    intermediate_vars=[
                        out.MatchIntermediateVar(
                            location=out.Location(
                                path=out.Fpath("foo.py"),
                                start=out.Position(13, 14, 16),
                                end=out.Position(13, 15, 17),
                            ),
                            content="??",
                        )
                    ],
                    taint_sink=out.MatchCallTrace(
                        out.CliLoc(
                            (
                                out.Location(
                                    path=out.Fpath("foo.py"),
                                    start=out.Position(15, 16, 20),
                                    end=out.Position(15, 17, 21),
                                ),
                                "??",
                            )
                        )
                    ),
                ),
                engine_kind=out.EngineKind(out.OSS()),
            ),
        ),
    )
    return match


@pytest.mark.quick
def test_dataflow_source_to_thread_flow_sarif(mocker):
    # https://docs.oasis-open.org/sarif/sarif/v2.1.0/cs01/sarif-v2.1.0-cs01.html#_Toc16012707
    file_content = dedent(
        """
        # first line
        def foo():
            5 == 5 # nosem
        """
    ).lstrip()
    mocker.patch.object(Path, "open", mocker.mock_open(read_data=file_content))
    mocker.patch.object(builtins, "open", mocker.mock_open(read_data=file_content))
    taint_rule_match = create_taint_rule_match()
    thread_flow_location = SarifFormatter._taint_source_to_thread_flow_location_sarif(
        taint_rule_match
    )

    assert bool(thread_flow_location.get("location")), (
        "If location information is available, a threadFlowLocation object "
        "SHALL contain a property named location."
    )
    assert isinstance(thread_flow_location.get("location"), dict), (
        "A location value is a location object that specifies the location to "
        "which the threadFlowLocation object refers"
    )


@pytest.mark.quick
def test_intermediate_vars_to_thread_flow_location_sarif(mocker):
    # https://docs.oasis-open.org/sarif/sarif/v2.1.0/cs01/sarif-v2.1.0-cs01.html#_Toc16012707
    file_content = dedent(
        """
        # first line
        def foo():
            5 == 5 # nosem
        """
    ).lstrip()
    mocker.patch.object(Path, "open", mocker.mock_open(read_data=file_content))
    mocker.patch.object(builtins, "open", mocker.mock_open(read_data=file_content))
    taint_rule_match = create_taint_rule_match()
    thread_flow_locations = (
        SarifFormatter._intermediate_vars_to_thread_flow_location_sarif(
            taint_rule_match
        )
    )

    assert isinstance(thread_flow_locations, list), (
        "A threadFlow object SHALL contain a property named locations whose "
        "value is an array of one or more threadFlowLocation objects "
    )

    for thread_flow_location in thread_flow_locations:
        assert bool(thread_flow_location.get("location")), (
            "If location information is available, a threadFlowLocation object "
            "SHALL contain a property named location."
        )
        assert isinstance(thread_flow_location.get("location"), dict), (
            "A location value is a location object that specifies the location to "
            "which the threadFlowLocation object refers"
        )


@pytest.mark.quick
def test_sink_to_thread_flow_location_sarif(mocker):
    # https://docs.oasis-open.org/sarif/sarif/v2.1.0/cs01/sarif-v2.1.0-cs01.html#_Toc16012707
    file_content = dedent(
        """
        # first line
        def foo():
            5 == 5 # nosem
        """
    ).lstrip()
    mocker.patch.object(Path, "open", mocker.mock_open(read_data=file_content))
    mocker.patch.object(builtins, "open", mocker.mock_open(read_data=file_content))
    taint_rule_match = create_taint_rule_match()
    thread_flow_location = SarifFormatter._sink_to_thread_flow_location_sarif(
        taint_rule_match
    )

    assert bool(thread_flow_location.get("location")), (
        "If location information is available, a threadFlowLocation object "
        "SHALL contain a property named location."
    )
    assert isinstance(thread_flow_location.get("location"), dict), (
        "A location value is a location object that specifies the location to "
        "which the threadFlowLocation object refers"
    )


@pytest.mark.quick
def test_dataflow_trace_to_thread_flows_sarif(mocker):
    # https://docs.oasis-open.org/sarif/sarif/v2.1.0/cs01/sarif-v2.1.0-cs01.html#_Toc16012699
    file_content = dedent(
        """
        # first line
        def foo():
            5 == 5 # nosem
        """
    ).lstrip()
    mocker.patch.object(Path, "open", mocker.mock_open(read_data=file_content))
    mocker.patch.object(builtins, "open", mocker.mock_open(read_data=file_content))
    taint_rule_match = create_taint_rule_match()
    thread_flows = SarifFormatter._dataflow_trace_to_thread_flows_sarif(
        taint_rule_match
    )

    for thread_flow in thread_flows:
        assert bool(
            thread_flow.get("locations")
        ), "A threadFlow object SHALL contain a property named locations."
        assert isinstance(
            thread_flow.get("locations"), list
        ), "A locations value is an array of one or more threadFlowLocation objects"


@pytest.mark.quick
def test_dataflow_trace_to_codeflow_sarif(mocker):
    # https://docs.oasis-open.org/sarif/sarif/v2.1.0/cs01/sarif-v2.1.0-cs01.html#_Toc16012696
    file_content = dedent(
        """
        # first line
        def foo():
            5 == 5 # nosem
        """
    ).lstrip()
    mocker.patch.object(Path, "open", mocker.mock_open(read_data=file_content))
    mocker.patch.object(builtins, "open", mocker.mock_open(read_data=file_content))
    taint_rule_match = create_taint_rule_match()
    code_flow = SarifFormatter._dataflow_trace_to_codeflow_sarif(taint_rule_match)

    assert bool(
        code_flow.get("threadFlows")
    ), "A codeFlow object SHALL contain a property named threadFlows"
    assert isinstance(
        code_flow.get("threadFlows"), list
    ), "A threadFlows value is an array of one or more threadFlow objects"


@pytest.mark.quick
def test_rule_to_sarif_tags():
    r = """
      id: blah
      languages: [js]
      severity: INFO
      message: blah
      pattern: blah(...)
      metadata:
        cwe:
        - CWE-22
        owasp:
        - A01:2021
    """
    with StringIO(r) as stream:
        j = yaml.load(stream)
    rule = Rule.from_yamltree(YamlTree.wrap(j, EmptySpan))
    tags = SarifFormatter._rule_to_sarif_tags(rule)

    assert all([isinstance(tag, str) for tag in tags])
