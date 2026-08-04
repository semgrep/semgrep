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

# Semgrep Guardian

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

[Semgrep](https://semgrep.dev) Guardian integrates natively with AI coding agents like Claude Code and Cursor to catch security issues before they ship. It bundles the Semgrep [MCP server](https://modelcontextprotocol.io/docs/getting-started/intro), Hooks, and Skills into a single install, and scans every file an agent generates using Semgrep Code, Supply Chain, and Secrets. When findings are detected, the agent is prompted to regenerate code until Semgrep returns clean results or you choose to dismiss them.

[Model Context Protocol (MCP)](https://modelcontextprotocol.io/) is a standardized API for LLMs, Agents, and IDEs like Claude Code, Cursor, VS Code, Windsurf, or anything that supports MCP, to get specialized help, get context, and harness the power of tools. Semgrep is a fast, deterministic static analysis tool that semantically understands many [languages](https://semgrep.dev/docs/supported-languages) and comes with over [10,000 rules](https://semgrep.dev/registry). 🛠️

> [!NOTE]
> This project is under active development. We would love your feedback. Join the `#mcp` [community Slack](https://go.semgrep.dev/slack) channel!



## Contents

- [Semgrep Guardian](#semgrep-guardian)
  - [Getting started](#getting-started)
    - [Demo Video](#demo-video)
    - [Claude Code](#claude-code)
    - [Cursor](#cursor)

  - [Usage](#usage)
    - [Standard Input/Output (stdio)](#standard-inputoutput-stdio)
      - [Python](#python)
      - [Docker](#docker)
    - [Streamable HTTP](#streamable-http)
      - [Python](#python-1)
      - [Docker](#docker-1)
  - [Semgrep AppSec Platform](#semgrep-appsec-platform)
  - [Integrations](#integrations)
    - [Claude Code](#claude-code-integration)
    - [Cursor](#cursor-integration)
    - [VS Code / Copilot](#vs-code--copilot)
    - [Windsurf](#windsurf)
    - [Kiro](#kiro)
    - [Custom clients](#custom-clients)
      - [Example Python streamable HTTP client](#example-python-streamable-http-client)
  - [Contributing, community, and running from source](#contributing-community-and-running-from-source)
    - [Community projects 🌟](#community-projects-)
    - [MCP server registries](#mcp-server-registries)

## Getting started

### Demo Video

<video src="https://github.com/user-attachments/assets/4d034519-ad96-4c81-a024-0328979a2353" controls="controls">
</video>

### Claude Code

1. Start a new Claude Code instance in the terminal:
    ```bash
    claude
    ```

2.  Open the plugin marketplace:
    ```bash
    /plugin
    ```

3.  Go to **Discover**, search for **Semgrep**, and click **Install**.

4.  Set up the Semgrep plugin by running the following skill. This also installs the Semgrep CLI:
    ```bash
    /setup-semgrep-plugin
    ```

### Cursor

1. Open Cursor

2. Find Semgrep in the [Cursor Plugin Marketplace](https://cursor.com/marketplace/semgrep), or open `Cursor > ⌘⇧J > Plugins` and Search "Semgrep" and click **Add to Cursor**.

3.  Set up the Semgrep plugin by running the following skill. This also installs the Semgrep CLI:
    ```bash
    /setup-semgrep-plugin
    ```

4. Restart Cursor to apply configuration.



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

  Supports stdio and streamable-http transports. For stdio, it will read
  from stdin and write to stdout. For streamable-http, it will start
  an HTTP server on port 8000.

Options:
  -v, --version                   Show version and exit.
  -t, --transport [stdio|streamable-http]
                                  Transport protocol to use:
                                  stdio or streamable-http
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

### Claude Code Integration

1. Start a new Claude Code instance in the terminal:
    ```bash
    claude
    ```

2.  Open the plugin marketplace:
    ```bash
    /plugin
    ```

3.  Go to **Discover**, search for **Semgrep**, and click **Install**.

4.  Set up the Semgrep plugin by running the following skill. This also installs the Semgrep CLI:
    ```bash
    /setup-semgrep-plugin
    ```

See [Claude Code docs](https://docs.anthropic.com/en/docs/claude-code/tutorials#set-up-model-context-protocol-mcp) for more info.


### Cursor Integration

1. Open Cursor

2. Find Semgrep in the [Cursor Plugin Marketplace](https://cursor.com/marketplace/semgrep), or open `Cursor > ⌘⇧J > Plugins` and Search "Semgrep" and click **Add to Cursor**.

3.  Set up the Semgrep plugin by running the following skill. This also installs the Semgrep CLI:
    ```bash
    /setup-semgrep-plugin
    ```

4. Restart Cursor to apply configuration.

See [cursor docs](https://docs.cursor.com/context/model-context-protocol) for more info.

### VS Code / Copilot

#### Manual Configuration
Add the following JSON block to your User Settings (JSON) file in VS Code. You can do this by pressing `Ctrl + Shift + P` and typing Preferences: Open User Settings (JSON).
```
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
Optionally, you can add it to a file called .vscode/mcp.json in your workspace:
```
{
  "servers": {
    "semgrep": {
      "command": "semgrep",
        "args": ["mcp"]
    }
  }
}
```

See [VS Code docs](https://code.visualstudio.com/docs/copilot/chat/mcp-servers) for more info.

### Windsurf

1. Install Semgrep:
     ```bash
    # install through homebrew
    brew install semgrep
    ```

     ```bash
    # install through pip
    python3 -m pip install semgrep
    ```

2. Verify that you've installed the [latest version](https://github.com/semgrep/semgrep/releases) of Semgrep by running the following:
    ```bash
    semgrep --version
    ```

3. Log in to Semgrep and install Semgrep Pro:

    ```
    semgrep login && semgrep install-semgrep-pro
    ```

4. Create a `hooks.json` file at `~/.codeium/windsurf/hooks.json` and paste the following configuration:

    ```json
    {
      "hooks": {
        "post_write_code": [
          {
            "command": "semgrep mcp -k post-tool-cli-scan -a windsurf",
            "show_output": true
          }
        ]
      }
    }
    ```

5. Restart Windsurf to apply hook configuration.

See [Windsurf docs](https://docs.windsurf.com/windsurf/mcp) for more info.

### Kiro

Runs Semgrep locally using the CLI. Clicking the box below opens directly in Kiro to add the server — it does not open a new browser tab:

[![Add to Kiro](https://kiro.dev/images/add-to-kiro.svg)](https://kiro.dev/launch/mcp/add?name=semgrep&config=%7B%22command%22%3A%22semgrep%22%2C%22args%22%3A%5B%22mcp%22%5D%2C%22env%22%3A%7B%22SEMGREP_APP_TOKEN%22%3A%22%24%7BSEMGREP_TOKEN%7D%22%7D%7D)

Alternatively, follow the [Kiro MCP docs](https://kiro.dev/docs/mcp/) and add this file to your `.kiro/settings/mcp.json`:

```json
{
  "mcpServers": {
    "semgrep": {
      "command": "semgrep",
      "args": ["mcp"],
      "env": {
        "SEMGREP_APP_TOKEN": "${SEMGREP_TOKEN}"
      }
    }
  }
}
```

See [Kiro docs](https://kiro.dev/docs/mcp/) for more info.

### Custom clients

#### Example Python streamable HTTP client

```python
import asyncio
import json
from mcp.client.session import ClientSession
from mcp.client.streamable_http import streamablehttp_client


async def main():
    async with streamablehttp_client("http://localhost:8000/mcp") as (read_stream, write_stream, _):
        async with ClientSession(read_stream, write_stream) as session:
            await session.initialize()
            results = await session.call_tool(
                "semgrep_scan_remote",
                {
                    "code_files": [
                        {
                            "path": "hello_world.py",
                            "content": "def hello(): print('Hello, World!')",
                        }
                    ]
                },
            )
            content_block = results.content[0]
            content = json.loads(content_block.text)
            paths = content.get("paths", None)
            if paths:
                scanned = paths.get("scanned", [])
                findings = content.get("results", [])
                print(f"Scanned {len(scanned)} paths. Found {len(findings)} findings.")
```

> [!TIP]
> Some client libraries want the `URL`: [http://localhost:8000/mcp](http://localhost:8000/mcp)
> and others only want the `HOST`: `localhost:8000`.
> Try out the `URL` in a web browser to confirm the server is running, and there are no network issues.
> Set `SEMGREP_IS_HOSTED=true` to use the `semgrep_scan_remote` tool

See [official SDK docs](https://modelcontextprotocol.io/clients#adding-mcp-support-to-your-application) for more info.

## Contributing, community, and running from source

> [!NOTE]
> We love your feedback, bug reports, feature requests, and code. Join the `#mcp` [community Slack](https://go.semgrep.dev/slack) channel!

See [CONTRIBUTING.md](CONTRIBUTING.md) for more info and details on how to run from the MCP server from source code.


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
