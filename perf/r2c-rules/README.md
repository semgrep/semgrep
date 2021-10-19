The rules in this folder are advertised rule packs on the front page of https://semgrep.dev/explore. Each one was obtained via the command

```
curl https://semgrep.dev/c/p/[name] > [name].yml
```

To run using one against a corpus, have the prep file copy it into the input directory for that corpus

NOTE: I've commented one rule in r2c-ci.yml (react-css-injection) which was taking too long
on vulnerable apps because it was using a 'pattern: $STYLE' which leads
to TooManyMatches errors (code 12).
