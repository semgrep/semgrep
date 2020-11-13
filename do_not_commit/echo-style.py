import click

# ruleid:use-click-secho
click.echo(click.style("foo"))


# ruleid:use-click-secho
click.echo(click.style("foo", color="blue"))


# ok
click.secho("foo")
