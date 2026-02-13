#
# Copyright (c) 2025 Semgrep Inc.
#
# This library is free software; you can redistribute it and/or
# modify it under the terms of the GNU Lesser General Public License
# version 2.1 as published by the Free Software Foundation.
#
# This library is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the file
# LICENSE for more details.
#
from datetime import datetime
from typing import Any

from pydantic import BaseModel
from pydantic import ConfigDict
from pydantic import Field
from pydantic import HttpUrl


class FindingElicitationSchema(BaseModel):
    true_positive: bool = Field(description="The finding is a true positive.")
    false_positive: bool = Field(description="The finding is a false positive.")
    skip: bool = Field(description="Skip flagging this finding.")


class CodeFile(BaseModel):
    # This "path" is mostly for bookkeeping purposes.
    # Depending on whether the server is hosted or not, this path might
    # not actually exist on the filesystem.
    path: str = Field(description="Path of the code file")
    # The `content` field will be filled in either by the LLM (in the remote scanning case)
    # or gleaned from the filesystem (in the local scanning case).
    content: str = Field(description="Content of the code file")


class CodePath(BaseModel):
    path: str = Field(description="Absolute path of the code file")


class CodeWithLanguage(BaseModel):
    content: str = Field(description="Content of the code file")
    language: str = Field(
        description="Programing language of the code file", default="python"
    )


class WhoamiResult(BaseModel):
    id: int = Field(description="ID of the current user")
    name: str = Field(description="Name of the current user")
    email: str = Field(description="Email of the current user")
    login: str = Field(description="Login of the current user")


class SemgrepScanResult(BaseModel):
    version: str = Field(description="Version of Semgrep used for the scan")
    results: list[dict[str, Any]] = Field(description="List of semgrep scan results")
    errors: list[dict[str, Any]] = Field(
        description="List of errors encountered during scan", default_factory=list
    )
    paths: dict[str, Any] = Field(description="Paths of the scanned files")
    skipped_rules: list[str] = Field(
        description="List of rules that were skipped during scan", default_factory=list
    )
    mcp_scan_results: dict[str, Any] = Field(
        description="MCP scan results", default_factory=dict
    )


#
# Findings / Issues (protos.issues.v1) models
#


class ExternalTicket(BaseModel):
    """`ticketing.v1.ExternalTicket` in the semgrep-app repo (modeled loosely; exact fields owned by ticketing proto)."""

    model_config = ConfigDict(extra="allow", populate_by_name=True)
    id: int | None = None
    url: str | HttpUrl | None = None
    external_slug: str | None = Field(default=None, validation_alias="externalSlug")


class PrimaryRefItem(BaseModel):
    """`Issue.PrimaryRefItem` in the semgrep-app repo (modeled loosely; exact fields owned by issues proto)."""

    model_config = ConfigDict(extra="allow", populate_by_name=True)
    id: int | None = None
    ref: str | None = None


class Repository(BaseModel):
    """`Issue.Repository` in the semgrep-app repo (modeled loosely; exact fields owned by issues proto)."""

    model_config = ConfigDict(extra="allow", populate_by_name=True)
    name: str | None = None
    id: int | None = None
    type: str | None = None
    primary_ref: PrimaryRefItem | None = Field(
        default=None, validation_alias="primaryRef"
    )


class Scan(BaseModel):
    """`Issue.Scan` in the semgrep-app repo (modeled loosely; exact fields owned by issues proto)."""

    model_config = ConfigDict(extra="allow", populate_by_name=True)
    id: int | None = None
    meta: dict[str, Any] | None = None


class CodeSnippet(BaseModel):
    """`Issue.CodeSnippet` in the semgrep-app repo (modeled loosely; exact fields owned by issues proto)."""

    model_config = ConfigDict(extra="allow", populate_by_name=True)
    path: str | None = None
    content: str | None = None


class Finding(BaseModel):
    """`protos.issues.v1.Issue` in the semgrep-app repo (modeled loosely; exact fields owned by issues proto)."""

    model_config = ConfigDict(extra="allow", populate_by_name=True)
    # IDs
    id: int
    created_at: datetime | None = Field(default=None, validation_alias="createdAt")
    ref: str | None = None
    syntactic_id: str | None = Field(default=None, validation_alias="syntacticId")
    match_based_id: str | None = Field(default=None, validation_alias="matchBasedId")
    rule_id: int | None = Field(default=None, validation_alias="ruleId")
    # Status and triage
    status: str | None = None
    triage_state: str | None = Field(default=None, validation_alias="triageState")
    triage_reason: str | None = Field(default=None, validation_alias="triageReason")
    relevant_since: datetime | None = Field(
        default=None, validation_alias="relevantSince"
    )
    aggregate_state: str | None = Field(default=None, validation_alias="aggregateState")
    # Repo and scan context
    repository: Repository | None = None
    first_seen_scan: Scan | None = Field(default=None, validation_alias="firstSeenScan")
    last_seen_scan: Scan | None = Field(default=None, validation_alias="lastSeenScan")
    # Location
    line_of_code_url: str | None = Field(default=None, validation_alias="lineOfCodeUrl")
    file_path: str | None = Field(default=None, validation_alias="filePath")
    line: int | None = None
    end_line: int | None = Field(default=None, validation_alias="endLine")
    column: int | None = None
    end_column: int | None = Field(default=None, validation_alias="endColumn")
    # Rule
    severity: str | None = None
    message: str | None = None
    rule_path: str | None = Field(default=None, validation_alias="rulePath")
    confidence: str | None = None
    rule_url: str | None = Field(default=None, validation_alias="ruleUrl")
    # Preserve old fields that the semgrep_findings MCP tool used to return
    external_ticket: ExternalTicket | None = Field(
        default=None, validation_alias="externalTicket"
    )
