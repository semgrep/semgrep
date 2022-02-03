from dir1.a import A
from dir1.b import B

FORMATTERS: Mapping[OutputFormat, Type[BaseFormatter]] = {
    OutputFormat.EMACS: A,
    OutputFormat.VIM: B,
}
