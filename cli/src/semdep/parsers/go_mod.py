"""
Parser for go.mod files
Based on https://go.dev/ref/mod#go-mod-file
"""
from pathlib import Path
from typing import Dict
from typing import List
from typing import Optional
from typing import Tuple
from typing import TypeVar

from semdep.external.parsy import alt
from semdep.external.parsy import Parser
from semdep.external.parsy import regex
from semdep.external.parsy import string
from semdep.parsers.util import mark_line
from semdep.parsers.util import pair
from semdep.parsers.util import safe_path_parse
from semgrep.semgrep_interfaces.semgrep_output_v1 import Direct
from semgrep.semgrep_interfaces.semgrep_output_v1 import Ecosystem
from semgrep.semgrep_interfaces.semgrep_output_v1 import FoundDependency
from semgrep.semgrep_interfaces.semgrep_output_v1 import Gomod
from semgrep.semgrep_interfaces.semgrep_output_v1 import Transitive
from semgrep.semgrep_interfaces.semgrep_output_v1 import Transitivity

A = TypeVar("A")
B = TypeVar("B")


consume_line = regex(r"[^\n)]*").result(None)

comment = regex(r" *//([^\n]*)", flags=0, group=1)


def multi_spec(spec: "Parser[A]") -> "Parser[List[Tuple[A,Optional[str]]]]":
    return (
        regex(r"[ \t]*\(\n")
        >> (
            regex(r"[ \t]*") >> pair(spec, comment.optional(None)) << string("\n")
        ).many()
        << string(")")
    ) | (regex(r"[ \t]*") >> pair(spec, comment.optional()).map(lambda x: [x]))


def make_directive(
    dir: "Parser[A]", spec: "Parser[B]"
) -> "Parser[Tuple[A,List[Tuple[B,Optional[str]]]]]":
    return pair(dir, multi_spec(spec))


dep_spec = regex(r"([^ \n]+) v([^ \n]+)", flags=0, group=(1, 2))

specs: Dict[str, "Parser[Optional[Tuple[str,...]]]"] = {
    "module": consume_line,
    "go": consume_line,
    "require": dep_spec,
    "exclude": dep_spec,
    "replace": consume_line,
    "retract": consume_line,
}

directive = alt(
    *(make_directive(string(dir), mark_line(spec)) for dir, spec in specs.items())
)

go_mod = (
    directive.sep_by((comment.optional() >> string("\n")).at_least(1))
    << (comment.optional() >> string("\n")).many()
)


def parse_go_mod(
    lockfile_path: Path, manifest_path: Optional[Path]
) -> List[FoundDependency]:
    specs = safe_path_parse(lockfile_path, go_mod)
    if not specs:
        return []
    exclude = set()
    output = []
    for dir, data in specs:
        if dir == "exclude":
            for ((_, dep), _) in data:
                if dep:
                    package, version = dep
                    exclude.add((package, version))
        if dir == "require":
            for ((line_number, dep), comment) in data:
                if dep:
                    package, version = dep
                    output.append(
                        FoundDependency(
                            package=package,
                            version=version,
                            ecosystem=Ecosystem(Gomod()),
                            allowed_hashes={},
                            transitivity=Transitivity(
                                Transitive() if comment == " indirect" else Direct()
                            ),
                            line_number=line_number,
                            resolved_url=package,  # Go package names are URLs
                        )
                    )
    return [d for d in output if (d.package, d.version) not in exclude]


text = """\
module github.com/k8snetworkplumbingwg/whereabouts

go 1.19

require (
	github.com/blang/semver v3.5.1+incompatible
	github.com/containernetworking/cni v0.8.1
	github.com/containernetworking/plugins v0.8.2
	github.com/go-co-op/gocron v1.13.0
	github.com/imdario/mergo v0.3.12
	github.com/json-iterator/go v1.1.12 // indirect
	github.com/k8snetworkplumbingwg/network-attachment-definition-client v1.1.1-0.20210510153419-66a699ae3b05
	github.com/onsi/ginkgo v1.16.5
	github.com/onsi/gomega v1.17.0
	github.com/pkg/errors v0.9.1
	gomodules.xyz/jsonpatch/v2 v2.2.0
	k8s.io/api v0.24.0
	k8s.io/apimachinery v0.24.0
	k8s.io/client-go v0.24.0
	k8s.io/code-generator v0.24.0
	k8s.io/kube-openapi v0.0.0-20220413171646-5e7f5fdc6da6
)

require (
	github.com/robfig/cron/v3 v3.0.1 // indirect
	golang.org/x/sync v0.0.0-20220722155255-886fb9371eb4 // indirect
	sigs.k8s.io/json v0.0.0-20211208200746-9f7c6b3444d2 // indirect
)

require (
	github.com/davecgh/go-spew v1.1.1 // indirect
	github.com/emicklei/go-restful v2.16.0+incompatible // indirect
	github.com/evanphx/json-patch v5.6.0+incompatible // indirect
	github.com/fsnotify/fsnotify v1.5.4 // indirect
	github.com/go-logr/logr v1.2.3 // indirect
	github.com/go-openapi/jsonpointer v0.19.5 // indirect
	github.com/go-openapi/jsonreference v0.20.0 // indirect
	github.com/go-openapi/swag v0.21.1 // indirect
	github.com/gogo/protobuf v1.3.2 // indirect
	github.com/golang/groupcache v0.0.0-20210331224755-41bb18bfe9da // indirect
	github.com/golang/protobuf v1.5.2 // indirect
	github.com/google/gnostic v0.6.9 // indirect
	github.com/google/go-cmp v0.5.8 // indirect
	github.com/google/gofuzz v1.2.0 // indirect
	github.com/google/uuid v1.3.0 // indirect
	github.com/josharian/intern v1.0.0 // indirect
	github.com/mailru/easyjson v0.7.7 // indirect
	github.com/modern-go/concurrent v0.0.0-20180306012644-bacd9c7ef1dd // indirect
	github.com/modern-go/reflect2 v1.0.2 // indirect
	github.com/munnerz/goautoneg v0.0.0-20191010083416-a7dc8b61c822 // indirect
	github.com/nxadm/tail v1.4.8 // indirect
	github.com/spf13/pflag v1.0.5 // indirect
	golang.org/x/mod v0.6.0-dev.0.20220419223038-86c51ed26bb4 // indirect
	golang.org/x/net v0.7.0 // indirect
	golang.org/x/oauth2 v0.0.0-20220411215720-9780585627b5 // indirect
	golang.org/x/sys v0.5.0 // indirect
	golang.org/x/term v0.5.0 // indirect
	golang.org/x/text v0.7.0 // indirect
	golang.org/x/time v0.0.0-20220411224347-583f2d630306 // indirect
	golang.org/x/tools v0.1.12 // indirect
	google.golang.org/appengine v1.6.7 // indirect
	google.golang.org/protobuf v1.28.0 // indirect
	gopkg.in/inf.v0 v0.9.1 // indirect
	gopkg.in/tomb.v1 v1.0.0-20141024135613-dd632973f1e7 // indirect
	gopkg.in/yaml.v2 v2.4.0 // indirect
	gopkg.in/yaml.v3 v3.0.1 // indirect
	k8s.io/gengo v0.0.0-20211129171323-c02415ce4185 // indirect
	k8s.io/klog/v2 v2.60.1 // indirect
	k8s.io/utils v0.0.0-20220210201930-3a6ce19ff2f9
	sigs.k8s.io/structured-merge-diff/v4 v4.2.1 // indirect
	sigs.k8s.io/yaml v1.3.0 // indirect
)

replace github.com/gogo/protobuf => github.com/gogo/protobuf v1.3.2
"""
