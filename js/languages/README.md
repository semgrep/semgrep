We didn't port yet all the Semgrep languages to JS.
The one we ported are used only to get an idea of the size
of the different parsers depending on the technology used.

lua: small grammar, tree-sitter only
csharp: big grammar, tree-sitter only
js/ts: pfff + tree-sitter
