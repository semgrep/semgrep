from typing import Dict
from typing import List
from typing import Optional
from typing import Set
from typing import Tuple

from parsy import Parser
from parsy import string
from parsy import success

from semdep.parsers.util import any_str
from semdep.parsers.util import line_number
from semdep.parsers.util import not_any
from semdep.parsers.util import transitivity
from semgrep.semgrep_interfaces.semgrep_output_v1 import Ecosystem
from semgrep.semgrep_interfaces.semgrep_output_v1 import FoundDependency
from semgrep.semgrep_interfaces.semgrep_output_v1 import Pypi


comment = string(" ").many() >> string("#") >> not_any(["\n"])
newline = comment.optional() >> string("\n")

extra_info = not_any(["\\\\", "\n"])

package = not_any(["=", "<", ">", " ", "\n"])
version = not_any([";", " ", "\n"])


def dep(sep: "Parser[str]") -> "Parser[Tuple[str,str]]":
    return package.bind(
        lambda package: print("Package" + repr(package))
        or sep
        >> version.bind(
            lambda version: print("Version" + repr(version))
            or extra_info.optional() >> success((package, version))
        )
    )


manifest_dep = dep(any_str(["==", "<=", ">=", ">", "<"])) | package.map(
    lambda x: (x, "")
)

manifest = (
    (manifest_dep << newline.many()).map(lambda t: t[0]).many().map(lambda x: set(x))
)

hash = string("    --hash=sha256:") >> not_any([" ", "\n"])

hashes = hash.sep_by(string(" \\") >> newline.many())

empty_hashes: Dict[str, List[str]] = {}


def dep_hashes(manifest_deps: Optional[Set[str]]) -> "Parser[FoundDependency]":
    return dep(string("==")).bind(
        lambda dep: line_number.bind(
            lambda line_number: string("\\").optional()
            >> newline
            >> hashes.map(lambda hashes: {"sha256": hashes})
            .optional(default=empty_hashes)
            .bind(
                lambda hashes: success(
                    FoundDependency(
                        package=dep[0],
                        version=dep[1],
                        ecosystem=Ecosystem(Pypi()),
                        allowed_hashes=hashes,
                        transitivity=transitivity(manifest_deps, [dep[0]]),
                        line_number=line_number,
                    )
                )
            )
        )
    )


def requirements(
    manifest_deps: Optional[Set[str]],
) -> "Parser[List[FoundDependency]]":
    return (dep_hashes(manifest_deps) << newline.many()).many()


def parse_requirements(
    lockfile_text: str, manifest_text: Optional[str]
) -> List[FoundDependency]:
    manifest_deps = manifest.parse(manifest_text) if manifest_text else None
    return requirements(manifest_deps).parse(lockfile_text)


manifest_text = """\
bunch==1.0.1 # foo
# foo
coverage==6.3.3
dominate==2.6.0
numpy==1.23.2
pandas #Comment
plotly
psutil==5.9.0
pylint<=2.13.9
pytest>=7.1.2
pytest-forked==1.4.0
pytest-xdist==2.5.0
tqdm==4.64.0
wcmatch==8.3
"""

text = r"""pyobjc-framework-coremidi==8.1; python_version >= "3.6" and sys_platform == "darwin" \
    --hash=sha256:4f401444068d4ca8dc4a472b6de14b346e988af27af99c124d0478cb03da1d0c \
    --hash=sha256:c896ea1ca59b47d93777c7de092ca93f9ab7a00578f89805a2cea331ac98b545 \
    --hash=sha256:60b2f966701962747a6c3ad089c8be7a61376e77ce37fec5225aeb5bd0747e7f \
    --hash=sha256:4a30919f4ac52a2f8d7fd1c656b2a322396e42070f78dd6b884b5c0bd2b27576
pyobjc-framework-coreml==8.1; python_version >= "3.6" and sys_platform == "darwin" and platform_release >= "17.0" \
    --hash=sha256:f92ad9adf271aa8d10d46e8fd98f8e851709218409a22cab12a8ebcaa4f8b334 \
    --hash=sha256:711e6120d2413a9e2b05dd8a713c85ee689ac57a9ec3818d0ea4cddc96466253 \
    --hash=sha256:653d5b7c4de2d075d0175366f1de300c13dd2fc1da525ba77edce9f3400e4697 \
    --hash=sha256:f95934e052d21fea239fcab02a9b648e72050c95485dcc6250cde0cfa7c2fb35
pyobjc-framework-coremotion==8.1; python_version >= "3.6" and sys_platform == "darwin" and platform_release >= "19.0" \
    --hash=sha256:0afa1f4c624c2c2c3e2c01aee07fb14fb4bf81ea100a5da42c8f24b3a27b6da6 \
    --hash=sha256:3539f840268723bef310049b3ebc0d075b3b6aa9233c8b132829da616609b791
pyobjc-framework-coreservices==8.1; python_version >= "3.6" and sys_platform == "darwin" and platform_release >= "9.0" \
    --hash=sha256:c45207ffc5f583e9519561414298f713d299a6fe659ffff3d2a1554ae46111c6 \
    --hash=sha256:ceb1af4bb4da6347fb8a99c2ef5d88145b962fd87514fbf0aeca4717e584443c \
    --hash=sha256:a7d09ebc17fb7870edcab2ad30f56ab62bcabd069d1094557fb29f726ab19f83 \
    --hash=sha256:6fad2ce0a5f8cf7042716aa8b451c61416ae7a97165b6cc29cad4f2dce2de0fa
pyobjc-framework-corespotlight==8.1; python_version >= "3.6" and sys_platform == "darwin" and platform_release >= "17.0" \
    --hash=sha256:207473bbe84be9b4ac52c451128fa4dce3e3c88b6c7549217734f8ffc16dd000 \
    --hash=sha256:81af31dc9c63502ea8e0d70a64e9684538b35b7012dc5d77b02fdaa0c7625f35 \
    --hash=sha256:58b2848d5d497ac2450936b45242e5ac25ce028ea6d597cdf45df43de1477d7f \
    --hash=sha256:981444f0cb1f1271c1e9cda3e4b94051cda324bdf85081b60c26c28d93b3ae8d
pyobjc-framework-coretext==8.1; python_version >= "3.6" and sys_platform == "darwin" \
    --hash=sha256:fa2b544936520114370bf075ba467f1da8ac9fd805b52d26eba6073edb1c92c4 \
    --hash=sha256:1e577e31ed802b75876165a83b538d4c00485cc4ea6f51008fed0be91a365a4c \
    --hash=sha256:902e167f3111e31c66510a6a312680111aca05c12b408485aa5fa3b148979e80 \
    --hash=sha256:0e8c44c1539295baf1a77140ccf83f2e7f18dbe8347ef726029ed790b0a4679c \
    --hash=sha256:65a6baef03280e8bb9073bdb5201e052ae548b32d288a6f7b0c881724327f22b \
    --hash=sha256:0038100ddd54a35da9fe318db3cd5be937af2c9e8ecba93d4de844c8e5bf379b # Comment

pyobjc-framework-corewlan==8.1; python_version >= "3.6" and sys_platform == "darwin" and platform_release >= "10.0"
"""
