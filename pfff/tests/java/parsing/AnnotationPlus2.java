class Foo {

  @Option(
      name = "--output-format",
      usage =
          "Output format (default: list).\n"
              + " dot -  dot graph format.\n"
              + " dot_compact - dot graph format, compacted.\n"
              + " dot_bfs -  dot graph format in bfs order.\n"
              + " dot_bfs_compact - dot graph format in bfs order, compacted.\n"
              + " json - JSON format.\n"
              + " json_unconfigured - JSON format with unevaluated selects\n"
              + " thrift - thrift binary format.\n")
  protected OutputFormat outputFormat = OutputFormat.LIST;
}
