from semdep.external.parsy import any_char
from semdep.external.parsy import string
from semdep.external.parsy import success
from semdep.parsers.util import colon
from semdep.parsers.util import comma
from semdep.parsers.util import lbrace
from semdep.parsers.util import lbrack
from semdep.parsers.util import quoted_str
from semdep.parsers.util import rbrace
from semdep.parsers.util import rbrack
from semdep.parsers.util import upto
from semdep.parsers.util import whitespace

# :hex,
atom = string(":") >> upto(",", consume_other=False)

scm = atom
package_block = atom

# "2.7.0"
version_block = quoted_str

# [:mix], [], [:mix, :rebar3]
any_array = lbrack >> any_char.until(rbrack, consume_other=True)

# {:plug, "~> 1.14", [hex: :plug, repo: "hexpm", optional: false]}
inner_dependency_block = (
    lbrace >> atom >> comma >> quoted_str >> comma >> any_array >> rbrace
)

# {:plug, "~> 1.14", [hex: :plug, repo: "hexpm", optional: false]}, {:octo_fetch, "~> 0.3", [hex: :octo_fetch, repo: "hexpm", optional: false]}
many_independency_blocks = inner_dependency_block.sep_by(comma)


#   {
#     :hex,
#     :testing,
#     "1.0.1",
#     "1234bb4db5b32fc0f8aa5c4a2040348b4aa36687100fb8837b850e90cf60e06",
#     [:mix],
#     [],
#     "hexpm",
#     "98767a5d1c6c3e3d20497b03293be7f83b46f89a6f3987cc1f9262d299f1eaa7"
#   }
package_entry_value_block = (
    whitespace
    >> lbrace
    >> atom  # scm
    >> comma
    >> atom.bind(  # package name
        lambda package: (
            comma
            >> version_block.bind(
                lambda version: success((package, version))
            )  # version
        )
    )
    << comma
    << quoted_str  # hash
    << comma
    << any_array  # options
    << comma
    << lbrack
    << many_independency_blocks  # dependencies
    << rbrack
    << comma
    << quoted_str  # package manager
    << comma
    << quoted_str  # hash
    << rbrace
    << comma.optional()
)

package_key_value_block = whitespace >> quoted_str >> colon >> package_entry_value_block

many_package_blocks = package_key_value_block.sep_by(whitespace)

lockfile_parser = (
    whitespace >> string("%") >> lbrace >> many_package_blocks << whitespace << rbrace
)
