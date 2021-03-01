OnCall Standup Email Assistant (OCSEA, or OXY, a breath of fresh air)

### Overview

The goal of the programs in this directory is to assist the OnCall person
in generating the OnCall Update email Matt started to send.

### Usage

First you need to generate a github authentification token to
access the github API. Go to https://github.com/settings/tokens and
generate a new token. You'll need to give this token a few
capabilities. See https://docs.github.com/en/graphql/guides/forming-calls-with-graphql#authenticating-with-graphql

Then you can call `curl.shell` with this token as a parameter:
```sh
$ curl.shell $GITHUB_TOKEN > yesterday.json
```

Then a day after, you can generate the data for the current day with:
```sh
$ curl.shell $GITHUB_TOKEN > today.json
```

Finally you can run the small OCaml program to compare the current
state of the customer board with the one of yesterday. You should
then get an email template that you can refine:
```sh
$ dune build
$ ./_build/default/oncall.exe -base yesterday.json today.json

To Discuss (UNASSIGNED)

	- [story] Figma can run `nginx` rules on their codebase and report that the performance was good 
	<https://github.com/returntocorp/semgrep-app/issues/1736> ()

Unassigned To do

...
```

### Refining

If you want to improve the program, you probably first need to refine
the graphQL query. Follow the instructions at
https://docs.github.com/en/graphql/guides/using-the-explorer
to install the great GraphiQL interactive tool that greatly facilitates
the writing of a query.

You can then copy-paste the new query developed in GraphiQL in
curl.shell and refine oncall.ml to correctly parse the new query result.

Happy hacking.
