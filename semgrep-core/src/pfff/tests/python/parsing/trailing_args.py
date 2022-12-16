def foo():

  context.configure(
             connection=connection,
             target_metadata=target_metadata,
             process_revision_directives=process_revision_directives,
             **current_app.extensions["migrate"].configure_args,
  )
