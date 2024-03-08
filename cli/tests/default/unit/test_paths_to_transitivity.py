from pathlib import Path

import pytest

pytestmark = pytest.mark.kinda_slow

paths_to_transitivity = {
    "targets/dependency_aware/log4j/maven_dep_tree.txt": {
        "org.apache.logging.log4j:log4j-api": [],
        "org.apache.logging.log4j:log4j-core": [],
        "junit:junit": [],
        "org.apache.maven:maven-plugin-api": [
            {
                "package": "org.apache.maven:maven-model",
                "version": "3.8.6",
            },
            {
                "package": "org.apache.maven:maven-artifact",
                "version": "3.8.6",
            },
            {
                "package": "org.eclipse.sisu:org.eclipse.sisu.plexus",
                "version": "0.3.5",
            },
            {
                "package": "org.codehaus.plexus:plexus-utils",
                "version": "3.3.1",
            },
            {
                "package": "org.codehaus.plexus:plexus-classworlds",
                "version": "2.6.0",
            },
        ],
        "org.apache.maven:maven-model": [],
        "org.apache.maven:maven-artifact": [
            {"package": "org.apache.commons:commons-lang3", "version": "3.8.1"}
        ],
        "org.apache.commons:commons-lang3": [],
        "org.eclipse.sisu:org.eclipse.sisu.plexus": [
            {"package": "javax.annotation:javax.annotation-api", "version": "1.2"},
            {"package": "org.eclipse.sisu:org.eclipse.sisu.inject", "version": "0.3.5"},
            {
                "package": "org.codehaus.plexus:plexus-component-annotations",
                "version": "1.5.5",
            },
        ],
        "javax.annotation:javax.annotation-api": [],
        "org.eclipse.sisu:org.eclipse.sisu.inject": [],
        "org.codehaus.plexus:plexus-component-annotations": [],
        "org.codehaus.plexus:plexus-utils": [],
        "org.codehaus.plexus:plexus-classworlds": [],
        "org.apache.maven.plugin-tools:maven-plugin-annotations": [],
        "com.zaxxer:nuprocess": [
            {"package": "net.java.dev.jna:jna", "version": "5.11.0"}
        ],
        "net.java.dev.jna:jna": [],
    },
}


# These are file targets which are ecosystems that we can calculate the path to
# transitivity for
@pytest.mark.parametrize(
    "target_supports_path_to_transitivity",
    [
        "targets/dependency_aware/log4j/maven_dep_tree.txt",
    ],
)
def test_child_construction(
    parse_lockfile_path_in_tmp, target_supports_path_to_transitivity
):
    dependencies, error = parse_lockfile_path_in_tmp(
        Path(target_supports_path_to_transitivity)
    )

    """
    This might be weird but I wanted to create a simple test that people can reuse as we
    add more ecosystems. The idea is that we can just add a new target to the list above
    and then simultaneously add a new entry to the paths_to_transitivity dict in
    test_paths_to_transitivity.py. The key would be the new target and the value would
    be the expected result of the parse. Then we can just run the test and it will
    spit out the new expected result. This is a bit more manual than I'd like but it's
    the best I could come up with. If you have a better idea please let me know!

    This entire paragraph was pretty much generated by Copilot wow
    """

    desiredResult = paths_to_transitivity[target_supports_path_to_transitivity]
    assert len(error) == 0
    for dependency in dependencies:
        dependency_children = [child.to_json() for child in dependency.children]
        assert dependency_children == desiredResult[dependency.package]