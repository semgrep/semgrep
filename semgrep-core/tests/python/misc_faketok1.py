def foo():

    #ERROR: match
    click.echo(click.style(f"Detected project with {projects}\n", fg="blue", err=True))

    #TODO: (unfortunately?) the pattern will not detect that
    click.echo(f" $ {click.style(cmd, bold=True)}\n", err=True)
