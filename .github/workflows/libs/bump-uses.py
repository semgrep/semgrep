#!/usr/bin/env -S uv run --script
#
# /// script
# requires-python = ">=3.12"
# dependencies = [
#     "pydantic~=2.12",
#     "pydantic-extra-types~=2.10",
#     "pygithub~=2.8",
#     "rich==14.2",
# ]
# ///
import json
import os
from argparse import ArgumentParser

from github import Auth
from github import Github
from github import GithubException
from github import Repository
from github import Tag
from pydantic import BaseModel
from pydantic import ValidationError
from rich.console import Console

console = Console()


class Action(BaseModel):
    version: str
    name: str


class ActionOrg(BaseModel):
    actions: list[Action]
    name: str


class UsesFile(BaseModel):
    dependencies: dict[str, dict[str, str]]

    @classmethod
    def from_file(cls, filepath: str) -> "UsesFile":
        with open(filepath) as f:
            return cls.model_validate_json(f.read())

    @classmethod
    def from_action_orgs(cls, action_repos: list[ActionOrg]) -> "UsesFile":
        dependencies: dict[str, dict[str, str]] = {}
        for action_repo in action_repos:
            actions_dict: dict[str, str] = {}
            for action in action_repo.actions:
                actions_dict[action.name] = action.version
            dependencies[action_repo.name] = actions_dict
        return cls(dependencies=dependencies)

    def to_action_orgs(self) -> list[ActionOrg]:
        action_repos: list[ActionOrg] = []
        for repo_name, actions_dict in self.dependencies.items():
            actions = [
                Action(name=action_name, version=version)
                for action_name, version in actions_dict.items()
            ]
            action_repos.append(ActionOrg(name=repo_name, actions=actions))
        return action_repos


def get_github_client() -> Github:
    import subprocess

    auth_token = os.getenv("GITHUB_TOKEN")
    if auth_token:
        console.print("Using GITHUB_TOKEN for authentication")
        return Github(auth=Auth.Token(auth_token))

    try:
        result = subprocess.run(
            ["gh", "auth", "token"],
            capture_output=True,
            text=True,
            timeout=10,
        )
        if result.returncode == 0:
            token = result.stdout.strip()
            if token:
                console.print("Using token from [bold]gh auth token[/bold]")
                return Github(auth=Auth.Token(token))
    except (FileNotFoundError, subprocess.TimeoutExpired):
        pass

    console.print(
        "[yellow]Warning: No GitHub token found, using unauthenticated access[/yellow]"
    )
    return Github()


def find_tag(g: Github, repo_full_name: str, tag: str) -> Tag.Tag | None:
    try:
        repo: Repository.Repository = g.get_repo(repo_full_name)
    except GithubException.UnknownObjectException:
        exit_with_error(f"Repository {repo_full_name} not found")
    tags = repo.get_tags()
    for t in tags:
        if t.name == tag:
            return t
    return None


def make_argparser() -> ArgumentParser:
    parser = ArgumentParser(description="Bump uses lockfile")
    # --dry-run - only output what would be changed
    parser.add_argument(
        "-d",
        "--dry-run",
        action="store_true",
        help="If set, do not output changes to useslockfile.json",
    )
    repo_org_group = parser.add_mutually_exclusive_group()
    # --org - only bump for this org
    repo_org_group.add_argument(
        "-o",
        "--org",
        type=str,
        help="If set, only bump for this org",
    )
    # --repo - only bump for this repo
    repo_org_group.add_argument(
        "-r",
        "--repo",
        type=str,
        help="If set, only bump for this repo (format: org/repo)",
    )

    return parser


def exit_with_error(message: str) -> None:
    console.print(f":cross_mark: [bold red]{message}, exiting...[/bold red]")
    exit(1)


def validate_uses(deps: list[ActionOrg]) -> None:
    # ensure no duplicate orgs, and no duplicate actions within orgs
    org_names: set[str] = set()
    for org in deps:
        if org.name in org_names:
            exit_with_error(f"Duplicate org found: {org.name} in uses.json, please fix")
        org_names.add(org.name)

        action_names: set[str] = set()
        for action in org.actions:
            if action.name in action_names:
                exit_with_error(
                    f"Duplicate action found in org {org.name}: {action.name} in uses.json, please fix"
                )
            action_names.add(action.name)


def main() -> None:
    # parser
    parser = make_argparser().parse_args()
    g = get_github_client()

    # load in uses.jsonn
    try:
        uses: list[ActionOrg] = UsesFile.from_file("uses.json").to_action_orgs()
        uses_lockfile: list[ActionOrg] = UsesFile.from_file(
            "useslockfile.json"
        ).to_action_orgs()
    except FileNotFoundError as e:
        exit_with_error(f"Could not find file:\n{e}")
    except ValidationError as e:
        exit_with_error(f"Invalid uses.json or useslockfile.json:\n{e}")

    console.print("[bold]Validating uses.json...[/bold]")
    validate_uses(uses)
    console.print(":white_heavy_check_mark: uses.json is valid")
    console.print("[bold]Validating useslockfile.json...[/bold]")
    validate_uses(uses_lockfile)
    console.print(":white_heavy_check_mark: useslockfile.json is valid")
    print("")
    console.print("[bold]Checking which actions to update...[/bold]")
    to_update: list[ActionOrg] = []
    # Here we don't worry about duplicates, as uses.json is assumed to be valid after
    if parser.org:
        console.print(f"Only updating org: {parser.org}")
        # try and find the org in uses
        org = [o for o in uses if o.name == parser.org]
        if not org:
            exit_with_error(f"Org {parser.org} not found in uses.json")
            exit(1)
    elif parser.repo:
        console.print(f"Only updating repo: [underline]{parser.repo}[/underline]")
        # check if format is org/repo
        if "/" not in parser.repo:
            exit_with_error(f"Invalid repo format: {parser.repo}, please use org/repo")

        org_name, repo_name = parser.repo.split("/", 1)
        if not org_name or not repo_name:
            exit_with_error(f"Invalid repo format: {parser.repo}, please use org/repo")

        org = [o for o in uses if o.name == org_name]
        if not org:
            exit_with_error(f"Org {org_name} not found in uses.json")
        action_name = [a for a in org[0].actions if a.name == repo_name]
        if not action_name:
            exit_with_error(
                f"Repo {repo_name} not found in uses.json under org {org_name}"
            )
        to_update = [ActionOrg(name=org_name, actions=action_name)]
    else:
        console.print("No org or repo filter provided, checking all actions")
        to_update = uses
    console.print(f"[bold]Going to check the following actions for updates:[/bold]")
    for org in to_update:
        for action in org.actions:
            console.print(
                f"- [underline]{org.name}/{action.name}@{action.version}[/underline]"
            )
    console.print("")

    console.print("[bold]Checking for updates...[/bold]")
    for org in to_update:
        lockfile_org = next(
            (o for o in uses_lockfile if o.name == org.name),
            ActionOrg(name=org.name, actions=[]),
        )
        for action in org.actions:
            action_name = f"{org.name}/{action.name}"
            lockfile_action = next(
                (a for a in lockfile_org.actions if a.name == action.name),
                Action(name=action.name, version="UNSET"),
            )

            tag: Tag.Tag | None = find_tag(g, action_name, action.version)
            if not tag:
                exit_with_error(
                    f"Tag {action.version} not found for {action_name} (https://github.com/{action_name}/tags)"
                )

            current_sha: str = tag.commit.sha
            if current_sha != lockfile_action.version:
                console.print(
                    f"- [underline]{action_name}@{action.version}[/underline] tag sha changed"
                )
                # Now update the lockfile org actions
                if lockfile_action not in lockfile_org.actions:
                    lockfile_org.actions.append(lockfile_action)
                    console.print("  action not pre-existing, added to lockfile")
                else:
                    console.print(
                        f"  diff: https://github.com/{action_name}/compare/{lockfile_action.version}...{current_sha}"
                    )
                    # replace existing action
                    lockfile_org.actions = [
                        lockfile_action if a.name == lockfile_action.name else a
                        for a in lockfile_org.actions
                    ]

                lockfile_action.version = current_sha
            else:
                console.print(
                    f"- [underline]{action_name}@{action.version}[/underline] sha unchanged"
                )
        # Now replace existing org in lockfile list if exists else append
        if lockfile_org not in uses_lockfile:
            uses_lockfile.append(lockfile_org)
        else:
            uses_lockfile = [
                lockfile_org if o.name == lockfile_org.name else o
                for o in uses_lockfile
            ]
    console.print("Finished checking all actions for updates\n")
    console.print("[bold]Checking for removed orgs/actions...[/bold]")
    # Now check for removed orgs/actions
    for lockfile_org in uses_lockfile[:]:
        uses_org = next(
            (o for o in uses if o.name == lockfile_org.name),
            None,
        )
        if not uses_org:
            console.print(f"- Org [underline]{lockfile_org.name}[/underline] removed")
            uses_lockfile.remove(lockfile_org)
            continue
        for lockfile_action in lockfile_org.actions[:]:
            uses_action = next(
                (a for a in uses_org.actions if a.name == lockfile_action.name),
                None,
            )
            if not uses_action:
                console.print(
                    f"- Action [underline]{lockfile_org.name}/{lockfile_action.name}[/underline] removed"
                )
                lockfile_org.actions.remove(lockfile_action)

    console.print("Finished checking for removed orgs/actions\n")
    new_uses_lockfile = UsesFile.from_action_orgs(uses_lockfile)
    # for sorted keys we have to do this
    # see: https://github.com/pydantic/pydantic/issues/7424
    instance_dict = new_uses_lockfile.model_dump()
    json_str = (
        json.dumps(
            instance_dict,
            indent=2,
            sort_keys=True,
        )
        + "\n"
    )
    if parser.dry_run:
        console.print("Dry run enabled, not writing changes to useslockfile.json")
        console.print("Would have wrote:")
        console.print(json_str)
    else:
        console.print("Writing changes to useslockfile.json")
        with open("useslockfile.json", "w") as f:
            f.write(json_str)
        console.print("Wrote changes to useslockfile.json")
    console.print("All done, please update the jsonnet files")


if __name__ == "__main__":
    main()
