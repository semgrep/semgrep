from typing import Any
from typing import List
from typing import Optional
from typing import Tuple

import inspect
import re
# Match https://github.com/pallets/click/blob/56db54650fd083cc35bc4891ffbda6e5e08e2762/src/click/core.py#L15C1-L16C1
from gettext import gettext as _

import click
from semgrep.constants import Colors
from semgrep.constants import DEFAULT_EPILOGUE, DEFAULT_PREAMBLE, CLI_DOCS_URL, SEMGREP_LOGO
from semgrep.console import console

# Inspired by https://github.com/pallets/click/issues/430#issuecomment-207580492
class DefaultGroup(click.Group):
    """
    Example Usage:

    python3 cmd.py <==> python3 cmd.py run
    python3 cmd.py --test foo bar <==> python3 cmd.py run --test foo bar

    cmd.py

    @click.group(cls=DefaultGroup, default_command="run")
    def cli():
        pass

    @cli.command()
    @click.option('--test')
    @click.option('--config')
    @click.option('--blah')
    def run(test, config, blah):
        click.echo("a")
        click.echo(test)

    @cli.command()
    def init()
        click.echo('The subcommand')

    cli()
    """

    ignore_unknown_options = True

    def __init__(self, *args: Any, **kwargs: Any) -> None:
        default_command = kwargs.pop("default_command", None)
        super().__init__(*args, **kwargs)
        self.default_command_name = None
        if default_command is not None:
            self.default_command_name = default_command
        self._help_command = "COMMAND --help"

    def parse_args(self, ctx: click.Context, args: List[str]) -> List[str]:
        """
        If there are no arguments, insert default command
        """
        if not args and self.default_command_name is not None:
            args.insert(0, self.default_command_name)
        return super().parse_args(ctx, args)

    def get_command(
        self, ctx: click.Context, command_name: str
    ) -> Optional[click.Command]:
        """
        If COMMAND_NAME is not in self.commands then it means
        it is an option/arg to the default command. So place
        this in the context for retrieval in resolve_command.

        Also replace COMMAND_NAME with the self.default_command_name

        """
        if command_name not in self.commands and self.default_command_name:
            ctx._default_command_overwrite_args0 = command_name  # type: ignore
            command_name = self.default_command_name

        return super().get_command(ctx, command_name)

    def resolve_command(
        self, ctx: click.Context, args: List[str]
    ) -> Tuple[Optional[str], Optional[click.Command], List[str]]:
        """
        MultiCommand.resolve_command assumes args[0] is the command name
        if we are running a default command then args[0] will actually be
        an option/arg to the default command.

        In get_command we put this first arg into _default_command_overwrite_arg0
        in the context. So here we just read it from context again

        If args[0] is actually a command name then _default_command_overwrite_args0
        will not be set so this function is equivalent to existing behavior
        """
        cmd_name, cmd, args = super().resolve_command(ctx, args)
        if hasattr(ctx, "_default_command_overwrite_args0"):
            args.insert(0, ctx._default_command_overwrite_args0)
        return cmd_name, cmd, args

    def format_epilog(self, ctx: click.Context, formatter: click.HelpFormatter) -> None:
        """
        Overrides super().format_epilog to include a colored link to our docs
        https://github.com/pallets/click/blob/56db54650fd083cc35bc4891ffbda6e5e08e2762/src/click/core.py#L1105
        """
        from semgrep.util import with_color  # avoiding circular imports
        if self.epilog:
            formatter.write_paragraph()
            if self.epilog == DEFAULT_EPILOGUE:
                message, eol = self.epilog.split(CLI_DOCS_URL)
                text = f"{message}{with_color(Colors.cyan, CLI_DOCS_URL, underline=True)}"
                formatter.write_dl([(text, "")]) # force single line for link
                return
            for line in self.epilog.split("\n"):
                formatter.write_text(line)

    def format_help_text(self, ctx: click.Context, formatter: click.HelpFormatter) -> None:
        """
        Overrides super().format_help_text to include our semgrep logo and add color to commands
        """
        from semgrep.util import with_color  # avoiding circular imports
        colored_logo = with_color(Colors.green, SEMGREP_LOGO)
        colored_preamble = DEFAULT_PREAMBLE.replace(SEMGREP_LOGO, colored_logo)
        formatter.write(colored_preamble)
        if self.help is not None:
            # truncate the help text to the first form feed
            text = inspect.cleandoc(self.help).partition("\f")[0]
            command_match = re.search("`[^`]*`", text)
            if command_match:
                command = command_match.group(0)
                text = text.replace(command, with_color(Colors.cyan, command))
            formatter.write_text(text)
            return
        # If there is no help text, we fall back to the default help text
        super().format_help_text(ctx, formatter)

    def format_usage(self, ctx: click.Context, formatter: click.HelpFormatter) -> None:
        """
        Overrides super().format_usage to omit the pre-built usage statement
        """
        return

    def format_help_section(self, ctx: click.Context, formatter: click.HelpFormatter) -> None:
        """
        Overrides super().format_help_section to show our custom help section
        """
        from semgrep.util import with_color  # avoiding circular imports
        help_cmd = with_color(Colors.cyan, f"semgrep {self._help_command}")
        rows = [ (help_cmd, "For more information on each command")]
        # NOTE: this col_spacing is a little hacky but it works well enough for now
        col_spacing = 2 + self.get_column_max_width(self.list_commands_pairs(ctx)) - len(self._help_command)
        with formatter.section(_(with_color(Colors.foreground, "Help", underline=True))):
            formatter.write_dl(rows, col_spacing=col_spacing)

    def list_commands_pairs(self, ctx: click.Context) -> List[Tuple[str, click.Command]]:
        """
        Helper to list the command objects and their corresponding names
        """
        commands = []
        for subcommand in self.list_commands(ctx):
            cmd = self.get_command(ctx, subcommand)
            # What is this, the tool lied about a command.  Ignore it
            if cmd is None:
                continue
            if cmd.hidden:
                continue
            commands.append((subcommand, cmd))
        return commands

    def get_column_max_width(self, commands: List[Tuple[str, click.Command]]) -> int:
        """
        Get the max width of the command names and the help text for consistent formatting
        """
        return max(len(self._help_command), max(len(cmd[0]) for cmd in commands))

    def format_commands(self, ctx: click.Context, formatter: click.HelpFormatter) -> None:
        """
        Overrides super().format_commands to add color and a prefix to the command name
        """
        from semgrep.util import with_color  # avoiding circular imports
        commands = self.list_commands_pairs(ctx)

        if len(commands):
            limit = formatter.width - 6 - self.get_column_max_width(commands)

            rows = []
            for subcommand, cmd in commands:
                help = cmd.get_short_help_str(limit)
                command_text = with_color(Colors.cyan, f"semgrep {subcommand}")
                rows.append((command_text, help))

            if rows:
                with formatter.section(_(with_color(Colors.foreground, "Commands", underline=True))):
                    formatter.write_dl(rows)

    def format_options(self, ctx: click.Context, formatter: click.HelpFormatter) -> None:
        """
        Overrides super().format_options to show the commands and then a help section
        """
        self.format_commands(ctx, formatter)
        self.format_help_section(ctx, formatter)

