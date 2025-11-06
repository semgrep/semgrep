<p align="center">
  <a href="https://semgrep.dev">
    <picture>
      <source media="(prefers-color-scheme: light)" srcset="images/semgrep-logo-light.svg">
      <source media="(prefers-color-scheme: dark)" srcset="images/semgrep-logo-dark.svg">
      <img src="https://raw.githubusercontent.com/semgrep/mcp/main/images/semgrep-logo-light.svg" height="60" alt="Semgrep logo"/>
    </picture>
  </a>
</p>
<p align="center">
  <a href="https://semgrep.dev/docs/">
      <img src="https://img.shields.io/badge/Semgrep-docs-2acfa6?style=flat-square" alt="Documentation" />
  </a>
  <a href="https://go.semgrep.dev/slack">
    <img src="https://img.shields.io/badge/Slack-4.5k%20-4A154B?style=flat-square&logo=slack&logoColor=white" alt="Join Semgrep community Slack" />
  </a>
  <a href="https://www.linkedin.com/company/semgrep/">
    <img src="https://img.shields.io/badge/LinkedIn-follow-0a66c2?style=flat-square" alt="Follow on LinkedIn" />
  </a>
  <a href="https://x.com/intent/follow?screen_name=semgrep">
    <img src="https://img.shields.io/badge/semgrep-000000?style=flat-square&logo=x&logoColor=white?style=flat-square" alt="Follow @semgrep on X" />
  </a>
</p>

# Semgrep MCP Server

<!-- Seems these probably don't work for the moment, after the port -->
<!-- [![Add MCP Server semgrep to LM Studio](https://files.lmstudio.ai/deeplink/mcp-install-light.svg)](https://lmstudio.ai/install-mcp?name=semgrep&config=eyJ1cmwiOiJodHRwczovL21jcC5zZW1ncmVwLmFpL21jcCIsImhlYWRlcnMiOnsiQXV0aG9yaXphdGlvbiI6IkJlYXJlciA8WU9VUl9IRl9UT0tFTj4ifX0%3D)
[![Install in Cursor](https://img.shields.io/badge/Cursor-uv-0098FF?style=flat-square)](cursor://anysphere.cursor-deeplink/mcp/install?name=semgrep&config=eyJjb21tYW5kIjoidXZ4IiwiYXJncyI6WyJzZW1ncmVwLW1jcCJdfQ==)
[![Install in VS Code UV](https://img.shields.io/badge/VS_Code-uv-0098FF?style=flat-square&logo=githubcopilot&logoColor=white)](https://insiders.vscode.dev/redirect/mcp/install?name=semgrep&config=%7B%22command%22%3A%22uvx%22%2C%22args%22%3A%5B%22semgrep-mcp%22%5D%7D)
[![Install in VS Code Docker](https://img.shields.io/badge/VS_Code-docker-0098FF?style=flat-square&logo=githubcopilot&logoColor=white)](https://insiders.vscode.dev/redirect/mcp/install?name=semgrep&config=%7B%22command%22%3A%22docker%22%2C%22args%22%3A%5B%22run%22%2C%20%22-i%22%2C%20%22--rm%22%2C%20%22ghcr.io%2Fsemgrep%2Fmcp%22%2C%20%22-t%22%2C%20%22stdio%22%5D%7D)
[![Install in VS Code semgrep.ai](https://img.shields.io/badge/VS_Code-semgrep.ai-0098FF?style=flat-square&logo=githubcopilot&logoColor=white)](https://insiders.vscode.dev/redirect/mcp/install?name=semgrep.ai&config=%7B%22type%22%3A%20%22sse%22%2C%20%22url%22%3A%22https%3A%2F%2Fmcp.semgrep.ai%2Fsse%22%7D)
[![PyPI](https://img.shields.io/pypi/v/semgrep-mcp?style=flat-square&color=blue&logo=python&logoColor=white)](https://pypi.org/project/semgrep-mcp/)
[![Docker](https://img.shields.io/badge/docker-ghcr.io%2Fsemgrep%2Fmcp-0098FF?style=flat-square&logo=docker&logoColor=white)](https://ghcr.io/semgrep/mcp)
[![Install in VS Code Insiders](https://img.shields.io/badge/VS_Code_Insiders-uv-24bfa5?style=flat-square&logo=githubcopilot&logoColor=white)](https://insiders.vscode.dev/redirect/mcp/install?name=semgrep&config=%7B%22command%22%3A%22uvx%22%2C%22args%22%3A%5B%22semgrep-mcp%22%5D%7D&quality=insiders)
[![Install in VS Code Insiders](https://img.shields.io/badge/VS_Code_Insiders-docker-24bfa5?style=flat-square&logo=githubcopilot&logoColor=white)](https://insiders.vscode.dev/redirect/mcp/install?name=semgrep&config=%7B%22command%22%3A%22docker%22%2C%22args%22%3A%5B%22run%22%2C%20%22-i%22%2C%20%22--rm%22%2C%20%22ghcr.io%2Fsemgrep%2Fmcp%22%2C%20%22-t%22%2C%20%22stdio%22%5D%7D&quality=insiders) -->

A Model Context Protocol (MCP) server for using [Semgrep](https://semgrep.dev) to scan code for security vulnerabilities. Secure your [vibe coding](https://semgrep.dev/blog/2025/giving-appsec-a-seat-at-the-vibe-coding-table/)! 😅

[Model Context Protocol (MCP)](https://modelcontextprotocol.io/) is a standardized API for LLMs, Agents, and IDEs like Cursor, VS Code, Windsurf, or anything that supports MCP, to get specialized help, get context, and harness the power of tools. Semgrep is a fast, deterministic static analysis tool that semantically understands many [languages](https://semgrep.dev/docs/supported-languages) and comes with over [5,000 rules](https://semgrep.dev/registry). 🛠️

> [!NOTE]
> This beta project is under active development. We would love your feedback, bug reports, feature requests, and code. Join the `#mcp` [community Slack](https://go.semgrep.dev/slack) channel!

## Contents

- [Semgrep MCP Server](#semgrep-mcp-server)
  - [Contents](#contents)
  - [Getting started](#getting-started)
    - [Cursor](#cursor)
    - [ChatGPT](#chatgpt)
    - [Hosted Server](#hosted-server)
      - [Cursor](#cursor-1)
  - [Demo](#demo)
  - [API](#api)
    - [Tools](#tools)
      - [Scan Code](#scan-code)
      - [Understand Code](#understand-code)
      - [Cloud Platform (login and Semgrep token required)](#cloud-platform-login-and-semgrep-token-required)
      - [Meta](#meta)
    - [Prompts](#prompts)
    - [Resources](#resources)
  - [Usage](#usage)
    - [Standard Input/Output (stdio)](#standard-inputoutput-stdio)
      - [Python](#python)
      - [Docker](#docker)
    - [Streamable HTTP](#streamable-http)
      - [Python](#python-1)
      - [Docker](#docker-1)
    - [Server-sent events (SSE)](#server-sent-events-sse)
      - [Python](#python-2)
      - [Docker](#docker-2)
  - [Semgrep AppSec Platform](#semgrep-appsec-platform)
  - [Integrations](#integrations)
    - [Cursor IDE](#cursor-ide)
    - [VS Code / Copilot](#vs-code--copilot)
      - [Manual Configuration](#manual-configuration)
      - [Using Docker](#using-docker)
    - [Windsurf](#windsurf)
    - [Claude Desktop](#claude-desktop)
    - [Claude Code](#claude-code)
    - [OpenAI](#openai)
      - [Agents SDK](#agents-sdk)
    - [Custom clients](#custom-clients)
      - [Example Python SSE client](#example-python-sse-client)
  - [Contributing, community, and running from source](#contributing-community-and-running-from-source)
    - [Similar tools 🔍](#similar-tools-)
    - [Community projects 🌟](#community-projects-)
    - [MCP server registries](#mcp-server-registries)

## Getting started

Install the Semgrep binary as described [elsewhere in this repository](https://github.com/semgrep/semgrep?tab=readme-ov-file#option-2-getting-started-from-the-cli), and use it to run the MCP server:

```bash
semgrep mcp # see --help for more options
```

Or, run as a [Docker container](https://ghcr.io/semgrep/mcp):

```bash
docker run -i --rm semgrep/semgrep semgrep mcp
```

### Cursor

Example [`mcp.json`](https://docs.cursor.com/context/model-context-protocol)

```json
{
  "mcpServers": {
    "semgrep": {
      "command": "semgrep",
      "args": ["mcp"],
      "env": {
        "SEMGREP_APP_TOKEN": "<token>"
      }
    }
  }
}

```

Add an instruction to your [`.cursor/rules`](https://docs.cursor.com/context/rules-for-ai) to use automatically:

```text
Always scan code generated using Semgrep for security vulnerabilities
```

### ChatGPT

1. Go to the **Connector Settings** page ([direct link](https://chatgpt.com/admin/ca#settings/ConnectorSettings?create-connector=true))
1. **Name** the connection `Semgrep`
1. Set **MCP Server URL** to `https://mcp.semgrep.ai/sse`
1. Set **Authentication** to `No authentication`
1. Check the **I trust this application** checkbox
1. Click **Create**

See more details at the [official docs](https://platform.openai.com/docs/mcp).


### Hosted Server

> [!WARNING]
> [mcp.semgrep.ai](https://mcp.semgrep.ai) is an experimental server that may break unexpectedly. It will rapidly gain new functionality.🚀

#### Cursor

1. **Cmd + Shift + J** to open Cursor Settings
1. Select **MCP Tools**
1. Click **New MCP Server**.
1.

```json
{
  "mcpServers": {
    "semgrep": {
      "type": "streamable-http",
      "url": "https://mcp.semgrep.ai/mcp"
    }
  }
}
```

## Demo

<a href="https://www.loom.com/share/8535d72e4cfc4e1eb1e03ea223a702df"> <img style="max-width:300px;" src="https://cdn.loom.com/sessions/thumbnails/8535d72e4cfc4e1eb1e03ea223a702df-1047fabea7261abb-full-play.gif"> </a>

## API

### Tools

Enable LLMs to perform actions, make deterministic computations, and interact with external services.

#### Scan Code

- `security_check`: Scan code for security vulnerabilities
- `semgrep_scan`: Scan code files for security vulnerabilities with a given config string
- `semgrep_scan_with_custom_rule`: Scan code files using a custom Semgrep rule

#### Understand Code

- `get_abstract_syntax_tree`: Output the Abstract Syntax Tree (AST) of code

#### Cloud Platform (login and Semgrep token required)
- `semgrep_findings`: Fetch Semgrep findings from the Semgrep AppSec Platform API

#### Meta

- `supported_languages`: Return the list of languages Semgrep supports
- `semgrep_rule_schema`: Fetches the latest semgrep rule JSON Schema

### Prompts

Reusable prompts to standardize common LLM interactions.

- `write_custom_semgrep_rule`: Return a prompt to help write a Semgrep rule

### Resources

Expose data and content to LLMs

- `semgrep://rule/schema`: Specification of the Semgrep rule YAML syntax using JSON schema
- `semgrep://rule/{rule_id}/yaml`: Full Semgrep rule in YAML format from the Semgrep registry

## Usage

In order to use the Semgrep MCP server, you must first have the Semgrep CLI:
```
$ brew install semgrep
```

The server can then be invoked via the `mcp` subcommand:

```text
$ semgrep mcp --help

Usage: semgrep mcp [OPTIONS]

  Entry point for the MCP server

  Supports stdio, streamable-http, and sse transports. For stdio, it will read
  from stdin and write to stdout. For streamable-http and sse, it will start
  an HTTP server on port 8000.

Options:
  -v, --version                   Show version and exit.
  -t, --transport [stdio|streamable-http|sse]
                                  Transport protocol to use: stdio,
                                  streamable-http, or sse (legacy)
  -p, --port INTEGER              Port to use for the MCP server
  -h, --help                      Show this message and exit.
```

### Standard Input/Output (stdio)

The stdio transport enables communication through standard input and output streams. This is particularly useful for local integrations and command-line tools. See the [spec](https://modelcontextprotocol.io/docs/concepts/transports#built-in-transport-types) for more details.

#### Python

```bash
semgrep mcp
```

By default, the server will run in `stdio` mode. Because it's using the standard input and output streams, it will look like the tool is hanging without any output, but this is expected.

#### Docker

The Semgrep binary is published to Docker:

```
docker run -i --rm semgrep/semgrep semgrep mcp -t stdio
```

### Streamable HTTP

Streamable HTTP enables streaming responses over JSON RPC via HTTP POST requests. See the [spec](https://modelcontextprotocol.io/specification/draft/basic/transports#streamable-http) for more details.

By default, the server listens on [127.0.0.1:8000/mcp](https://127.0.0.1/mcp) for client connections. To change any of this, set [FASTMCP\_\*](https://github.com/modelcontextprotocol/python-sdk/blob/71889d7387f070cd872cab7c9aa3d1ff1fa5a5d2/src/mcp/server/fastmcp/server.py#L59-L60) environment variables. _The server must be running for clients to connect to it._

#### Python

```bash
semgrep mcp -t streamable-http
```

By default, the server will run in `stdio` mode, so you will have to include `-t streamable-http`.

#### Docker

```
docker run -p 8000:8000 semgrep/semgrep semgrep mcp
```


### Server-Sent Events (SSE)

> [!WARNING]
> The MCP community considers this a legacy transport protocol and is really intended for backwards compatibility. [Streamable HTTP](#streamable-http) is the recommended replacement.

SSE transport enables server-to-client streaming with Server-Sent Events for client-to-server and server-to-client communication. See the [spec](https://modelcontextprotocol.io/docs/concepts/transports#server-sent-events-sse) for more details.

By default, the server listens on [127.0.0.1:8000/sse](https://127.0.0.1/sse) for client connections. To change any of this, set [FASTMCP\_\*](https://github.com/modelcontextprotocol/python-sdk/blob/71889d7387f070cd872cab7c9aa3d1ff1fa5a5d2/src/mcp/server/fastmcp/server.py#L59-L60) environment variables. _The server must be running for clients to connect to it._

#### Python

```bash
semgrep mcp -t sse
```

By default, the Python package will run in `stdio` mode, so you will have to include `-t sse`.

#### Docker

```
docker run -p 8000:0000 semgrep/semgrep semgrep mcp -t sse
```

## Semgrep AppSec Platform

Optionally, to connect to Semgrep AppSec Platform:

1. [Login](https://semgrep.dev/login/) or sign up
1. Generate a token from [Settings](https://semgrep.dev/orgs/-/settings/tokens/api)
1. Add the token to your environment variables:
   - CLI (`export SEMGREP_APP_TOKEN=<token>`)

   - Docker (`docker run -e SEMGREP_APP_TOKEN=<token>`)

   - MCP config JSON

```json
    "env": {
      "SEMGREP_APP_TOKEN": "<token>"
    }
```

> [!TIP]
> Please [reach out for support](https://semgrep.dev/docs/support) if needed. ☎️

## Integrations

### Cursor IDE

Add the following JSON block to your `~/.cursor/mcp.json` global or `.cursor/mcp.json` project-specific configuration file:

```json
{
  "mcpServers": {
    "semgrep": {
      "command": "semgrep",
      "args": ["mcp"]
    }
  }
}
```

![cursor MCP settings](/images/cursor.png)

See [cursor docs](https://docs.cursor.com/context/model-context-protocol) for more info.

### VS Code / Copilot

Click the install buttons at the top of this README for the quickest installation.

#### Manual Configuration

Add the following JSON block to your User Settings (JSON) file in VS Code. You can do this by pressing `Ctrl + Shift + P` and typing `Preferences: Open User Settings (JSON)`.

```json
{
  "mcp": {
    "servers": {
      "semgrep": {
        "command": "semgrep",
        "args": ["mcp"]
      }
    }
  }
}
```

Optionally, you can add it to a file called `.vscode/mcp.json` in your workspace:

```json
{
  "servers": {
    "semgrep": {
      "command": "semgrep",
        "args": ["mcp"]
    }
  }
}
```

#### Using Docker

```json
{
  "mcp": {
    "servers": {
      "semgrep": {
        "command": "docker",
        "args": [
          "run",
          "-i",
          "--rm",
          "semgrep/semgrep",
          "semgrep",
          "mcp",
          "-t",
          "stdio"
        ]
      }
    }
  }
}
```

See [VS Code docs](https://code.visualstudio.com/docs/copilot/chat/mcp-servers) for more info.

### Windsurf

Add the following JSON block to your `~/.codeium/windsurf/mcp_config.json` file:

```json
{
  "mcpServers": {
    "semgrep": {
      "command": "semgrep",
      "args": ["mcp"]
    }
  }
}
```

See [Windsurf docs](https://docs.windsurf.com/windsurf/mcp) for more info.

### Claude Desktop

Here is a [short video](https://www.loom.com/share/f4440cbbb5a24149ac17cc7ddcd95cfa) showing Claude Desktop using this server to write a custom rule.

Add the following JSON block to your `claude_desktop_config.json` file:

```json
{
  "mcpServers": {
    "semgrep": {
      "command": "semgrep",
      "args": ["mcp"]
    }
  }
}
```

See [Anthropic docs](https://docs.anthropic.com/en/docs/agents-and-tools/mcp) for more info.

### Claude Code

```bash
claude mcp add semgrep semgrep mcp
```

See [Claude Code docs](https://docs.anthropic.com/en/docs/claude-code/tutorials#set-up-model-context-protocol-mcp) for more info.

### OpenAI

See the official docs:
- https://platform.openai.com/docs/mcp
- https://platform.openai.com/docs/guides/tools-remote-mcp

#### Agents SDK

```python
async with MCPServerStdio(
    params={
        "command": "semgrep",
        "args": ["mcp"],
    }
) as server:
    tools = await server.list_tools()
```

See [OpenAI Agents SDK docs](https://openai.github.io/openai-agents-python/mcp/) for more info.

### Custom clients

#### Example Python SSE client

See a full example in [examples/sse_client.py](examples/sse_client.py)

```python
from mcp.client.session import ClientSession
from mcp.client.sse import sse_client


async def main():
    async with sse_client("http://localhost:8000/sse") as (read_stream, write_stream):
        async with ClientSession(read_stream, write_stream) as session:
            await session.initialize()
            results = await session.call_tool(
                "semgrep_scan",
                {
                    "code_files": [
                        {
                            "path": "hello_world.py",
                            "content": "def hello(): print('Hello, World!')",
                        }
                    ]
                },
            )
            print(results)
```

> [!TIP]
> Some client libraries want the `URL`: [http://localhost:8000/sse](http://localhost:8000/sse)
> and others only want the `HOST`: `localhost:8000`.
> Try out the `URL` in a web browser to confirm the server is running, and there are no network issues.

See [official SDK docs](https://modelcontextprotocol.io/clients#adding-mcp-support-to-your-application) for more info.

## Contributing, community, and running from source

> [!NOTE]
> We love your feedback, bug reports, feature requests, and code. Join the `#mcp` [community Slack](https://go.semgrep.dev/slack) channel!

See [CONTRIBUTING.md](CONTRIBUTING.md) for more info and details on how to run from the MCP server from source code.

### Similar tools 🔍

- [semgrep-vscode](https://github.com/semgrep/semgrep-vscode) - Official VS Code extension
- [semgrep-intellij](https://github.com/semgrep/semgrep-intellij) - IntelliJ plugin

### Community projects 🌟

- [semgrep-rules](https://github.com/semgrep/semgrep-rules) - The official collection of Semgrep rules
- [mcp-server-semgrep](https://github.com/Szowesgad/mcp-server-semgrep) - Original inspiration written by [Szowesgad](https://github.com/Szowesgad) and [stefanskiasan](https://github.com/stefanskiasan)

### MCP server registries

- [Glama](https://glama.ai/mcp/servers/@semgrep/mcp)

<a href="https://glama.ai/mcp/servers/@semgrep/mcp">
 <img width="380" height="200" src="https://glama.ai/mcp/servers/4iqti5mgde/badge" alt="Semgrep Server MCP server" />
 </a>

- [MCP.so](https://mcp.so/server/mcp/semgrep)

______________________________________________________________________

Made with ❤️ by the [Semgrep Team](https://semgrep.dev/about/)
