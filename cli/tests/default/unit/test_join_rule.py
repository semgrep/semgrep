import pytest

from semgrep.join_rule import Condition
from semgrep.join_rule import create_collection_set_from_conditions
from semgrep.join_rule import create_model_map
from semgrep.join_rule import InvalidConditionError
from semgrep.join_rule import JoinOperator
from semgrep.join_rule import model_factory


@pytest.mark.quick
@pytest.mark.parametrize(
    "A,propA,B,propB,op",
    [
        ("hello", "world", "goodbye", "world", JoinOperator("==")),
        ("hello", "world", "goodbye", "world", JoinOperator("!=")),
        ("hello", "world", "goodbye", "world", JoinOperator("~")),
        ("hello", "world", "goodbye", "world", JoinOperator("<")),
        ("hello", "world", "goodbye", "world", JoinOperator(">")),
        ("hello.hello", "$WORLD", "goodbye", "$WORLD", JoinOperator("==")),
        (
            "hello.hello.test.asdf.hello",
            "world",
            "goodbye",
            "world",
            JoinOperator("=="),
        ),
        (
            "hello.hello.test.asdf.hello",
            "world",
            "goodbye.goodbye",
            "world",
            JoinOperator("=="),
        ),
        ("hello-other-punc", "world", "goodbye.goodbye", "world", JoinOperator("==")),
    ],
)
def test_condition_parse(A, propA, B, propB, op):
    expected = Condition(A, propA, B, propB, op)

    condition_string = f"{A}.{propA} {op.value} {B}.{propB}"
    actual = Condition.parse(condition_string)

    assert expected == actual


@pytest.mark.quick
def test_condition_parse_dot_behavior():
    A = "a.b.c.d.e"
    propA = "$FOO"
    B = "f.g.h.i"
    propB = "$BAR"
    op = JoinOperator("==")

    condition_string = f"{A}.{propA} {op.value} {B}.{propB}"
    actual = Condition.parse(condition_string)

    assert actual.collection_a == A
    assert actual.property_a == propA
    assert actual.collection_b == B
    assert actual.property_b == propB
    assert actual.operator == op


@pytest.mark.quick
@pytest.mark.parametrize(
    "condition_string",
    [
        ("I'll do what I want!"),
        ("..."),
        ("$X"),
        ("hello.$X == "),
        ("hello.$X goodbye.$X"),
        ("hello.$X asdf goodbye.$X"),
    ],
)
def test_invalid_condition_string(condition_string):
    with pytest.raises(InvalidConditionError):
        Condition.parse(condition_string)


@pytest.mark.quick
def test_model_factory():
    model = model_factory("HelloWorld", ["a", "b", "c"])
    assert model.__name__ == "HelloWorld"
    assert model.a
    assert model.b
    assert model.c
    with pytest.raises(AttributeError):
        _ = model.d


@pytest.mark.quick
def test_create_collection_set_from_conditions():
    conditions = [
        Condition("A", "propA1", "B", "propB", JoinOperator("==")),
        Condition("A", "propA2", "C", "propC", JoinOperator("!=")),
    ]
    expected = {"A", "B", "C"}
    actual = create_collection_set_from_conditions(conditions)

    assert expected == actual


@pytest.mark.quick
def test_create_model_map():
    results = [
        {
            "check_id": "tests.e2e.rules.join_rules.rule_parts.flask-user-input",
            "end": {"col": 34, "line": 22},
            "extra": {
                "is_ignored": False,
                "lines": '    query = request.args.get("q")',
                "message": "query",
                "metadata": {},
                "metavars": {
                    "$SOMETHING": {
                        "abstract_content": "args",
                        "end": {"col": 25, "line": 22, "offset": 397},
                        "start": {"col": 21, "line": 22, "offset": 393},
                        "unique_id": {
                            "md5sum": "ab6ff1cb0dd043a2307ac57105f0030e",
                            "type": "AST",
                        },
                    },
                    "$VAR": {
                        "abstract_content": "query",
                        "end": {"col": 10, "line": 22, "offset": 382},
                        "start": {"col": 5, "line": 22, "offset": 377},
                        "unique_id": {"sid": 6, "type": "id"},
                    },
                },
                "severity": "INFO",
            },
            "path": "tests/default/e2e/targets/join_rules/user-input-escaped-with-safe/app.py",
            "start": {"col": 5, "line": 22},
        },
        {
            "check_id": "tests.e2e.rules.join_rules.rule_parts.render-template-input",
            "end": {"col": 86, "line": 31},
            "extra": {
                "is_ignored": False,
                "lines": '    return render_template("search.html", query=query, search_results=search_results)',
                "message": "search_results",
                "metadata": {},
                "metavars": {
                    "$INPUT": {
                        "abstract_content": "search_results",
                        "end": {"col": 85, "line": 31, "offset": 852},
                        "start": {"col": 71, "line": 31, "offset": 838},
                        "unique_id": {"sid": 7, "type": "id"},
                    },
                    "$TEMPLATE": {
                        "abstract_content": "search.html",
                        "end": {"col": 40, "line": 31, "offset": 807},
                        "start": {"col": 29, "line": 31, "offset": 796},
                        "unique_id": {
                            "md5sum": "f5f13723b9d579a8e2a5dbd2e894477f",
                            "type": "AST",
                        },
                    },
                    "$VAR": {
                        "abstract_content": "search_results",
                        "end": {"col": 70, "line": 31, "offset": 837},
                        "start": {"col": 56, "line": 31, "offset": 823},
                        "unique_id": {
                            "md5sum": "599271fb78fc6cc60ef9cb3cce78ebd5",
                            "type": "AST",
                        },
                    },
                },
                "severity": "INFO",
            },
            "path": "tests/default/e2e/targets/join_rules/user-input-escaped-with-safe/app.py",
            "start": {"col": 12, "line": 31},
        },
        {
            "check_id": "tests.e2e.rules.join_rules.rule_parts.render-template-input",
            "end": {"col": 86, "line": 31},
            "extra": {
                "is_ignored": False,
                "lines": '    return render_template("search.html", query=query, search_results=search_results)',
                "message": "query",
                "metadata": {},
                "metavars": {
                    "$INPUT": {
                        "abstract_content": "query",
                        "end": {"col": 54, "line": 31, "offset": 821},
                        "start": {"col": 49, "line": 31, "offset": 816},
                        "unique_id": {"sid": 6, "type": "id"},
                    },
                    "$TEMPLATE": {
                        "abstract_content": "search.html",
                        "end": {"col": 40, "line": 31, "offset": 807},
                        "start": {"col": 29, "line": 31, "offset": 796},
                        "unique_id": {
                            "md5sum": "f5f13723b9d579a8e2a5dbd2e894477f",
                            "type": "AST",
                        },
                    },
                    "$VAR": {
                        "abstract_content": "query",
                        "end": {"col": 48, "line": 31, "offset": 815},
                        "start": {"col": 43, "line": 31, "offset": 810},
                        "unique_id": {
                            "md5sum": "03bdbe50bee27f6f7f6803ab695d3212",
                            "type": "AST",
                        },
                    },
                },
                "severity": "INFO",
            },
            "path": "tests/default/e2e/targets/join_rules/user-input-escaped-with-safe/app.py",
            "start": {"col": 12, "line": 31},
        },
    ]
    model_map = create_model_map(results)

    check_ids = {result.get("check_id") for result in results}
    assert set(model_map.keys()) == check_ids
    for result in results:
        check_id = result.get("check_id")
        metavars = result.get("extra", {}).get("metavars")  # type: ignore
        for metavar in metavars.keys():
            assert getattr(model_map[check_id], metavar)  # type: ignore
