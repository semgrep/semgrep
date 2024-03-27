import pytest

from semdep.parsers import mix


@pytest.mark.parametrize(
    "original, output",
    [
        ('"2.7.0"', "2.7.0"),
        ('"1.2.0a-beta"', "1.2.0a-beta"),
    ],
)
@pytest.mark.no_semgrep_cli
@pytest.mark.osemfail
@pytest.mark.quick
def test_mix_version_block(original: str, output: str):
    result = mix.version_block.parse_partial(original)
    assert result[0] == output
    assert not result[1]


@pytest.mark.parametrize(
    "original, output",
    [
        (":testing,", "testing"),
        (":version2,", "version2"),
    ],
)
@pytest.mark.no_semgrep_cli
@pytest.mark.osemfail
@pytest.mark.quick
def test_mix_atom(original: str, output: str):
    result = mix.atom.parse_partial(original)
    assert result[0] == output
    assert result[1] == ","


@pytest.mark.parametrize(
    "original",
    [
        "[:mix]",
        "[]",
        "[:mix, :rebar3]",
    ],
)
@pytest.mark.no_semgrep_cli
@pytest.mark.osemfail
@pytest.mark.quick
def test_mix_options_block(original: str):
    result = mix.any_array.parse_partial(original)
    assert not result[1]


@pytest.mark.parametrize(
    "original, output",
    [
        (
            """{
            :hex,
            :poison,
            "4.0.2",
            "3e12b7abfc7b54df5b8f3ae8b6d4f9da1fa3c606ebf47dc7331a750c018f04fd",
            [:mix],
            [],
            "hexpm",
            "2aa8f95fbfb66c1ac62349f63dbb04ac36353953ef4b24ec3e193b53b8ccfe09"
        }""",
            (4, ("poison", "4.0.2")),
        ),
        (
            """{
            :hex,
            :telemetry,
            "0.4.3",
            "7b05fbb4db5b32fc0f8aa5c4a2040348b4aa36687100fb8837b850e90cf60e06",
            [:mix],
            [],
            "hexpm",
            "f6b37a5d1c6c3e3d20497b03293be7f83b46f89a6f3987cc1f9262d299f1eaa7"
        }""",
            (4, ("telemetry", "0.4.3")),
        ),
    ],
)
@pytest.mark.no_semgrep_cli
@pytest.mark.osemfail
@pytest.mark.quick
def test_mix_package_entry_value_block(original: str, output: str):
    result = mix.package_entry_value_block.parse_partial(original)
    assert result[0] == output
    assert not result[1]


@pytest.mark.parametrize(
    "original",
    [
        '{:plug, "~> 1.14", [hex: :plug, repo: "hexpm", optional: false]}',
        '{:octo_fetch, "~> 0.3", [hex: :octo_fetch, repo: "hexpm", optional: false]}',
    ],
)
@pytest.mark.no_semgrep_cli
@pytest.mark.osemfail
@pytest.mark.quick
def test_mix_inner_dependency_block(original: str):
    result = mix.inner_dependency_block.parse_partial(original)
    assert not result[1]


@pytest.mark.no_semgrep_cli
@pytest.mark.osemfail
@pytest.mark.quick
def test_mix_many_inner_dependency_blocks():
    original = '{:plug, "~> 1.14", [hex: :plug, repo: "hexpm", optional: false]}, {:octo_fetch, "~> 0.3", [hex: :octo_fetch, repo: "hexpm", optional: false]}'
    result = mix.many_independency_blocks.parse_partial(original)
    assert not result[1]


@pytest.mark.no_semgrep_cli
@pytest.mark.osemfail
@pytest.mark.quick
def test_mix_package_key_value_block():
    original = """
     "websock_adapter": {:hex, :websock_adapter, "0.5.5", "9dfeee8269b27e958a65b3e235b7e447769f66b5b5925385f5a569269164a210", [:mix], [{:bandit, ">= 0.6.0", [hex: :bandit, repo: "hexpm", optional: true]}, {:plug, "~> 1.14", [hex: :plug, repo: "hexpm", optional: false]}, {:plug_cowboy, "~> 2.6", [hex: :plug_cowboy, repo: "hexpm", optional: true]}, {:websock, "~> 0.5", [hex: :websock, repo: "hexpm", optional: false]}], "hexpm", "4b977ba4a01918acbf77045ff88de7f6972c2a009213c515a445c48f224ffce9"},
"""

    result = mix.package_key_value_block.parse_partial(original)
    assert result[0] == (2, ("websock_adapter", "0.5.5"))
    assert not result[1]


@pytest.mark.no_semgrep_cli
@pytest.mark.osemfail
@pytest.mark.quick
def test_mix_many_package_blocks():
    original = """
      "acceptor_pool": {:hex, :acceptor_pool, "1.0.0", "43c20d2acae35f0c2bcd64f9d2bde267e459f0f3fd23dab26485bf518c281b21", [:rebar3], [], "hexpm", "0cbcd83fdc8b9ad2eee2067ef8b91a14858a5883cb7cd800e6fcd5803e158788"},
      "bamboo": {:hex, :bamboo, "2.3.0", "d2392a2cabe91edf488553d3c70638b532e8db7b76b84b0a39e3dfe492ffd6fc", [:mix], [{:hackney, ">= 1.15.2", [hex: :hackney, repo: "hexpm", optional: false]}, {:jason, "~> 1.0", [hex: :jason, repo: "hexpm", optional: true]}, {:mime, "~> 1.4 or ~> 2.0", [hex: :mime, repo: "hexpm", optional: false]}, {:plug, "~> 1.0", [hex: :plug, repo: "hexpm", optional: false]}], "hexpm", "dd0037e68e108fd04d0e8773921512c940e35d981e097b5793543e3b2f9cd3f6"}
    """

    result = mix.many_package_blocks.parse_partial(original)
    assert result[0] == [(2, ("acceptor_pool", "1.0.0")), (3, ("bamboo", "2.3.0"))]
    assert not result[1]


@pytest.mark.no_semgrep_cli
@pytest.mark.osemfail
@pytest.mark.quick
def test_mix_lockfile_parser():
    full_lockfile = """%{
      "bamboo_smtp": {:hex, :bamboo_smtp, "4.2.2", "e9f57a2300df9cb496c48751bd7668a86a2b89aa2e79ccaa34e0c46a5f64c3ae", [:mix], [{:bamboo, "~> 2.2.0", [hex: :bamboo, repo: "hexpm", optional: false]}, {:gen_smtp, "~> 1.2.0", [hex: :gen_smtp, repo: "hexpm", optional: false]}], "hexpm", "28cac2ec8adaae02aed663bf68163992891a3b44cfd7ada0bebe3e09bed7207f"},
      "bypass": {:hex, :bypass, "2.1.0", "909782781bf8e20ee86a9cabde36b259d44af8b9f38756173e8f5e2e1fabb9b1", [:mix], [{:plug, "~> 1.7", [hex: :plug, repo: "hexpm", optional: false]}, {:plug_cowboy, "~> 2.0", [hex: :plug_cowboy, repo: "hexpm", optional: false]}, {:ranch, "~> 1.3", [hex: :ranch, repo: "hexpm", optional: false]}], "hexpm", "d9b5df8fa5b7a6efa08384e9bbecfe4ce61c77d28a4282f79e02f1ef78d96b80"},
      "castore": {:hex, :castore, "1.0.5", "9eeebb394cc9a0f3ae56b813459f990abb0a3dedee1be6b27fdb50301930502f", [:mix], [], "hexpm", "8d7c597c3e4a64c395980882d4bca3cebb8d74197c590dc272cfd3b6a6310578"},
      "cloak": {:hex, :cloak, "1.1.2", "7e0006c2b0b98d976d4f559080fabefd81f0e0a50a3c4b621f85ceeb563e80bb", [:mix], [{:jason, "~> 1.0", [hex: :jason, repo: "hexpm", optional: true]}], "hexpm", "940d5ac4fcd51b252930fd112e319ea5ae6ab540b722f3ca60a85666759b9585"},
      "combination": {:hex, :combination, "0.0.3", "746aedca63d833293ec6e835aa1f34974868829b1486b1e1cb0685f0b2ae1f41", [:mix], [], "hexpm", "72b099f463df42ef7dc6371d250c7070b57b6c5902853f69deb894f79eda18ca"},
      "cowboy": {:hex, :cowboy, "2.10.0", "ff9ffeff91dae4ae270dd975642997afe2a1179d94b1887863e43f681a203e26", [:make, :rebar3], [{:cowlib, "2.12.1", [hex: :cowlib, repo: "hexpm", optional: false]}, {:ranch, "1.8.0", [hex: :ranch, repo: "hexpm", optional: false]}], "hexpm", "3afdccb7183cc6f143cb14d3cf51fa00e53db9ec80cdcd525482f5e99bc41d6b"},
      "cowboy_telemetry": {:hex, :cowboy_telemetry, "0.4.0", "f239f68b588efa7707abce16a84d0d2acf3a0f50571f8bb7f56a15865aae820c", [:rebar3], [{:cowboy, "~> 2.7", [hex: :cowboy, repo: "hexpm", optional: false]}, {:telemetry, "~> 1.0", [hex: :telemetry, repo: "hexpm", optional: false]}], "hexpm", "7d98bac1ee4565d31b62d59f8823dfd8356a169e7fcbb83831b8a5397404c9de"},
      "credo": {:hex, :credo, "1.7.3", "05bb11eaf2f2b8db370ecaa6a6bda2ec49b2acd5e0418bc106b73b07128c0436", [:mix], [{:bunt, "~> 0.2.1 or ~> 1.0", [hex: :bunt, repo: "hexpm", optional: false]}, {:file_system, "~> 0.2 or ~> 1.0", [hex: :file_system, repo: "hexpm", optional: false]}, {:jason, "~> 1.0", [hex: :jason, repo: "hexpm", optional: false]}], "hexpm", "35ea675a094c934c22fb1dca3696f3c31f2728ae6ef5a53b5d648c11180a4535"},
      }
    """
    result = mix.lockfile_parser.parse_partial(full_lockfile)
    assert result[0] == [
        (2, ("bamboo_smtp", "4.2.2")),
        (3, ("bypass", "2.1.0")),
        (4, ("castore", "1.0.5")),
        (5, ("cloak", "1.1.2")),
        (6, ("combination", "0.0.3")),
        (7, ("cowboy", "2.10.0")),
        (8, ("cowboy_telemetry", "0.4.0")),
        (9, ("credo", "1.7.3")),
    ]
    assert not result[1]


@pytest.mark.parametrize(
    "original, output",
    [
        ('{:ehttpc, github: "emqx/ehttpc", tag: "0.4.13", override: true}', "ehttpc"),
        (
            '{:rocksdb, github: "emqx/erlang-rocksdb", tag: "1.8.0-emqx-2", override: true}',
            "rocksdb",
        ),
    ],
)
@pytest.mark.no_semgrep_cli
@pytest.mark.osemfail
@pytest.mark.quick
def test_mix_manifest_package_parser(original, output):
    result = mix.manifest_package.parse_partial(original)
    assert result[0] == output
    assert not result[1]


@pytest.mark.no_semgrep_cli
@pytest.mark.osemfail
@pytest.mark.quick
def test_mix_many_manifest_package_parser():
    original = """
    {:ehttpc, github: "emqx/ehttpc", tag: "0.4.13", override: true},
    {:gproc, github: "emqx/gproc", tag: "0.9.0.1", override: true}
    """

    result = mix.many_manifest_packages.parse_partial(original)
    assert result[0] == [(2, "ehttpc"), (3, "gproc")]
    assert not result[1]


@pytest.mark.parametrize(
    "original",
    ["defp deps(profile_info, version) do", "def deps (profile_info, version) do"],
)
@pytest.mark.no_semgrep_cli
@pytest.mark.osemfail
@pytest.mark.quick
def test_mix_manifest_deps_declaration(original):
    result = mix.manifest_deps_declaration.parse_partial(original)
    assert not result[1]


@pytest.mark.no_semgrep_cli
@pytest.mark.osemfail
@pytest.mark.quick
def test_mix_manifest_deps():
    original = """defp deps(profile_info, version) do
        [
          {:ehttpc, github: "emqx/ehttpc", tag: "0.4.13", override: true},
          {:gproc, github: "emqx/gproc", tag: "0.9.0.1", override: true},
          # some comment
          {:rocksdb, github: "emqx/erlang-rocksdb", tag: "1.8.0-emqx-2", override: true},
          {:grpc, github: "emqx/grpc-erl", tag: "0.6.12", override: true},
          {:ecpool, github: "emqx/ecpool", tag: "0.5.7", override: true},
          {:pbkdf2, github: "emqx/erlang-pbkdf2", tag: "2.0.4", override: true},
          {:typerefl, github: "ieQu1/typerefl", tag: "0.9.1", override: true}
        ]
      end
    """

    result = mix.manifest_deps.parse_partial(original)
    assert result[0] == [
        (3, "ehttpc"),
        (4, "gproc"),
        "          # some comment",
        (6, "rocksdb"),
        (7, "grpc"),
        (8, "ecpool"),
        (9, "pbkdf2"),
        (10, "typerefl"),
    ]
    assert not result[1]


@pytest.mark.no_semgrep_cli
@pytest.mark.osemfail
@pytest.mark.quick
def test_mix_manifest_parser():
    full_deps = """
        defmodule MyApp.MixProject do
          use Mix.Project

          def project do
            [
              name: "MyApp",
              source_url: "https://github.com/myapp/myapp",
              docs: docs(),
              app: :myapp,
              version: "1.1.1",
              elixir: "~> 1.14",
              deps: deps()
            ]
          end

          defp deps do
            [
              {:ehttpc, github: "emqx/ehttpc", tag: "0.4.13", override: true},
              {:gproc, github: "emqx/gproc", tag: "0.9.0.1", override: true},
              {:rocksdb, github: "emqx/erlang-rocksdb", tag: "1.8.0-emqx-2", override: true},
              {:grpc, github: "emqx/grpc-erl", tag: "0.6.12", override: true},
              {:ecpool, github: "emqx/ecpool", tag: "0.5.7", override: true},
              {:pbkdf2, github: "emqx/erlang-pbkdf2", tag: "2.0.4", override: true},
              {:typerefl, github: "ieQu1/typerefl", tag: "0.9.1", override: true}
            ]
          end
        end
    """
    result = mix.manifest_parser.parse_partial(full_deps)
    assert result[0] == [
        (19, "ehttpc"),
        (20, "gproc"),
        (21, "rocksdb"),
        (22, "grpc"),
        (23, "ecpool"),
        (24, "pbkdf2"),
        (25, "typerefl"),
    ]
    assert not result[1]
