all:
	dune build
clean:
	dune clean
test:
	dune runtest -f
install:
	dune install

#coupling: see .circleci/config.yml
#todo: --config semgrep_ocaml.yml
check:
	docker run --rm -v "${PWD}:/src" returntocorp/semgrep:develop --config semgrep.yml --exclude parsing_errors --exclude todo --exclude TODO_more --exclude _build --strict

check2:
	codecheck -with_graph_code graph_code.marshall -lang ml -filter 3 .


visual:
	codemap -screen_size 3 -filter pfff -efuns_client efuns_client -emacs_client /dev/null .
loc:
	codemap -no_legend -profile -screen_size 3 -filter pfff -test_loc .
index:
	codegraph_build -lang cmt -derived_data .

graph: index

.PHONY: all clean install test dump

# for really small changes, just push directly!
pr:
	git push origin `git rev-parse --abbrev-ref HEAD`
	hub pull-request -b develop -r mjambon -r IagoAbal -r emjin

push:
	git push origin `git rev-parse --abbrev-ref HEAD`

merge:
	A=`git rev-parse --abbrev-ref HEAD` && git checkout develop && git pull && git branch -D $$A
