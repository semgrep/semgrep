The rules in this folder are advertised rule packs on the front page of https://semgrep.dev/explore. Each one was obtained via the command

```
curl https://semgrep.dev/c/p/[name] > [name].yml
```

To run using one against a corpus, have the prep file copy it into the input directory for that corpus
