
for (const snippet of snippets) {
  snippet.data = fs.readFileSync(path.join(SNIPPETS_PATH, snippet.name),
                                 'utf8');
}
