  object stream extends Function1[Path, geny.Generator[Path]]{
    def apply(arg: Path) = new Generator[Path] {
      def generate(handleItem: Path => Generator.Action) = {
        val ds = Files.newDirectoryStream(arg.toNIO)
        val iter = ds.iterator()
        var currentAction: Generator.Action = Generator.Continue
        try {
          while (iter.hasNext && currentAction == Generator.Continue){
            currentAction = handleItem(Path(iter.next().toAbsolutePath))
          }
        } finally ds.close()
        currentAction
      }
    }
  }
