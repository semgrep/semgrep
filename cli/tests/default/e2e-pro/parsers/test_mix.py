import pytest

from semdep.parsers import mix

MIX_LOCK = """
%{
  "castore": {
    :hex,
    :castore,
    "0.1.22",
    "4127549e411bedd012ca3a308dede574f43819fe9394254ca55ab4895abfa1a2",
    [:mix],
    [],
    "hexpm",
    "c17576df47eb5aa1ee40cc4134316a99f5cad3e215d5c77b8dd3cfef12a22cac"
  },
  "cowboy": {
    :hex,
    :cowboy,
    "2.10.0",
    "ff9ffeff91dae4ae270dd975642997afe2a1179d94b1887863e43f681a203e26",
    [:make, :rebar3],
    [
      {:cowlib, "2.12.1", [hex: :cowlib, repo: "hexpm", optional: false]},
      {:ranch, "1.8.0", [hex: :ranch, repo: "hexpm", optional: false]}
    ],
    "hexpm",
    "3afdccb7183cc6f143cb14d3cf51fa00e53db9ec80cdcd525482f5e99bc41d6b"
  },
  "cowboy_telemetry": {
    :hex,
    :cowboy_telemetry,
    "0.4.0",
    "f239f68b588efa7707abce16a84d0d2acf3a0f50571f8bb7f56a15865aae820c",
    [:rebar3],
    [
      {:cowboy, "~> 2.7", [hex: :cowboy, repo: "hexpm", optional: false]},
      {:telemetry, "~> 1.0", [hex: :telemetry, repo: "hexpm", optional: false]}
    ],
    "hexpm",
    "7d98bac1ee4565d31b62d59f8823dfd8356a169e7fcbb83831b8a5397404c9de"
  },
  "cowlib": {
    :hex,
    :cowlib,
    "2.12.1",
    "a9fa9a625f1d2025fe6b462cb865881329b5caff8f1854d1cbc9f9533f00e1e1",
    [:make, :rebar3],
    [],
    "hexpm",
    "163b73f6367a7341b33c794c4e88e7dbfe6498ac42dcd69ef44c5bc5507c8db0"
  },
  "db_connection": {
    :hex,
    :db_connection,
    "2.5.0",
    "bb6d4f30d35ded97b29fe80d8bd6f928a1912ca1ff110831edcd238a1973652c",
    [:mix],
    [
      {:telemetry, "~> 0.4 or ~> 1.0", [hex: :telemetry, repo: "hexpm", optional: false]}
    ],
    "hexpm",
    "c92d5ba26cd69ead1ff7582dbb860adeedfff39774105a4f1c92cbb654b55aa2"
  },
  "decimal": {
    :hex,
    :decimal,
    "2.1.1",
    "5611dca5d4b2c3dd497dec8f68751f1f1a54755e8ed2a966c2633cf885973ad6",
    [:mix],
    [],
    "hexpm",
    "53cfe5f497ed0e7771ae1a475575603d77425099ba5faef9394932b35020ffcc"
  },
  "earmark_parser": {
    :hex,
    :earmark_parser,
    "1.4.39",
    "424642f8335b05bb9eb611aa1564c148a8ee35c9c8a8bba6e129d51a3e3c6769",
    [:mix],
    [],
    "hexpm",
    "06553a88d1f1846da9ef066b87b57c6f605552cfbe40d20bd8d59cc6bde41944"
  },
  "ecto": {
    :hex,
    :ecto,
    "3.10.1",
    "c6757101880e90acc6125b095853176a02da8f1afe056f91f1f90b80c9389822",
    [:mix],
    [
      {:decimal, "~> 1.6 or ~> 2.0", [hex: :decimal, repo: "hexpm", optional: false]},
      {:jason, "~> 1.0", [hex: :jason, repo: "hexpm", optional: true]},
      {:telemetry, "~> 0.4 or ~> 1.0", [hex: :telemetry, repo: "hexpm", optional: false]}
    ],
    "hexpm",
    "d2ac4255f1601bdf7ac74c0ed971102c6829dc158719b94bd30041bbad77f87a"
  },
  "ecto_sql": {
    :hex,
    :ecto_sql,
    "3.10.1",
    "6ea6b3036a0b0ca94c2a02613fd9f742614b5cfe494c41af2e6571bb034dd94c",
    [:mix],
    [
      {:db_connection, "~> 2.5 or ~> 2.4.1", [hex: :db_connection, repo: "hexpm", optional: false]},
      {:ecto, "~> 3.10.0", [hex: :ecto, repo: "hexpm", optional: false]},
      {:myxql, "~> 0.6.0", [hex: :myxql, repo: "hexpm", optional: true]},
      {:postgrex, "~> 0.16.0 or ~> 0.17.0 or ~> 1.0", [hex: :postgrex, repo: "hexpm", optional: true]},
      {:tds, "~> 2.1.1 or ~> 2.2", [hex: :tds, repo: "hexpm", optional: true]},
      {:telemetry, "~> 0.4.0 or ~> 1.0", [hex: :telemetry, repo: "hexpm", optional: false]}
    ],
    "hexpm",
    "31f6787ec0a54110c9968b8c89a2c86071e5e03b674b2e3d63b7b1f8ebf67b0b"
  },
  "ex_doc": {
    :hex,
    :ex_doc,
    "0.25.1",
    "e442b3058cfb203d4de7d471fa4fbdb9b429ebe1e7b2db1e556f9f612e87bb20",
    [:mix],
    [
      {:earmark_parser, "~> 1.4", [hex: :earmark_parser, repo: "hexpm", optional: false]},
      {:html_entities, "~> 0.5", [hex: :html_entities, repo: "hexpm", optional: true]},
      {:kronky, "~> 0.2", [hex: :kronky, repo: "hexpm", optional: true]},
      {:nimble_parsec, "~> 0.6.0", [hex: :nimble_parsec, repo: "hexpm", optional: true]},
      {:plug, "~> 1.7", [hex: :plug, repo: "hexpm", optional: false]},
      {:poison, "~> 3.0", [hex: :poison, repo: "hexpm", optional: true]}
    ],
    "hexpm",
    "3c5c180d2048040e867fb9c12b933f1509d9aa191eb44891b75194ff48d5e7f7"
  },
  "floki": {
    :hex,
    :floki,
    "0.31.0",
    "6f95dc2b1e3e2f1f4f8fb870651b8ae48ee055d3f8d45c8d714bdd3ff73c46f2",
    [:mix],
    [
      {:html_entities, "~> 0.5.0", [hex: :html_entities, repo: "hexpm", optional: false]},
      {:html_matcher, "~> 0.8.0", [hex: :html_matcher, repo: "hexpm", optional: false]},
      {:nimble_parsec, "~> 0.6 or ~> 0.5.1", [hex: :nimble_parsec, repo: "hexpm", optional: false]}
    ],
    "hexpm",
    "e6f45b70d6d6605b04e29b4b1654c8e5b628e35b5001694fd6d97f43a080ac49"
  },
  "html_entities": {
    :hex,
    :html_entities,
    "0.5.0",
    "3c431401c5d1ed42a2e57e07be2d07113fb0d169e76b77c23413b272c7d360b2",
    [:mix],
    [],
    "hexpm",
    "971740f4d3d42d2e1ee6d3c97b2f476ac25b6a348db23de4d5e5f9ff1da43e5c"
  },
  "html_matcher": {
    :hex,
    :html_matcher,
    "0.8.0",
    "de6696d635764f3b8961cb99a25a1f9d90539f8185dc2b6f4bb01ad34a0f853e",
    [:mix],
    [
      {:html_entities, "~> 0.4 or ~> 0.5.0", [hex: :html_entities, repo: "hexpm", optional: false]},
      {:nimble_parsec, "~> 0.6", [hex: :nimble_parsec, repo: "hexpm", optional: false]}
    ],
    "hexpm",
    "12a1aa6e6b94b88de71abbbcf96b42d43dd6ff89a0e6a646b9c2c9debaaf35f0"
  },
  "jason": {
    :hex,
    :jason,
    "1.2.2",
    "a62eecc5d1fd64c03c5f96d7d8f2f7a40e4a0a9dcf711b46a1df45ef6b58e233",
    [:mix],
    [],
    "hexpm",
    "dfb6e579e4a1a8a25b687451aedd176a4774f55b1569bfb4401b4b1c36ee4c1f"
  },
  "kronky": {
    :hex,
    :kronky,
    "0.2.1",
    "27a1696dd6ac31775e7f8f61b225ef79a640e5e40f07a6e441c163e3d3cb7c5f",
    [:mix],
    [
      {:poison, "~> 2.2", [hex: :poison, repo: "hexpm", optional: false]}
    ],
    "hexpm",
    "fc3e76d6cb13518085e0f956a219c4793272b98a5170b15d71ee2baf2b8ccdd8"
  },
  "myxql": {
    :hex,
    :myxql,
    "0.6.0",
    "f44c27bc9cb94a49798b90aa3d125b59c60e7e232e701d3f36de6d487dd421b1",
    [:mix],
    [],
    "hexpm",
    "dadeb4f6a198e19301f43b5bfb6e3e4b7b835f18947b8fe636e09ae9185dd3df"
  },
  "nimble_parsec": {
    :hex,
    :nimble_parsec,
    "0.6.0",
    "1b3b89a7b24ae153fca7ad1fb80181c51263fc4d158e9172d59354e93d7136de",
    [:mix],
    [],
    "hexpm",
    "2ac3e053456eb98c11d5dd22d3b90a9b56940d01300331e2923b75f702799b53"
  },
  "phoenix": {
    :hex,
    :phoenix,
    "1.6.5",
    "9e9a6de8b769a8e0b45aa7da529f1e58ababffad4cb03e121163dd08e3f6f5b5",
    [:mix],
    [
      {:jason, "~> 1.0 or ~> 1.2", [hex: :jason, repo: "hexpm", optional: false]},
      {:phoenix_html, "~> 3.0", [hex: :phoenix_html, repo: "hexpm", optional: false]},
      {:phoenix_pubsub, "~> 2.0", [hex: :phoenix_pubsub, repo: "hexpm", optional: false]},
      {:plug, "~> 1.0", [hex: :plug, repo: "hexpm", optional: false]},
      {:telemetry, "~> 0.4", [hex: :telemetry, repo: "hexpm", optional: false]}
    ],
    "hexpm",
    "c9f78e27e3e8b64d8f3c3fa289feef41bf91e2b08e48c3750c8a16618b5d4f7a"
  },
  "phoenix_html": {
    :hex,
    :phoenix_html,
    "3.0.0",
    "f8c06f50e80f69d9442b91e50f0be45fc0c3c004beccfb59eae5cf7c68b12326",
    [:mix],
    [
      {:jason, "~> 1.0 or ~> 1.2", [hex: :jason, repo: "hexpm", optional: false]}
    ],
    "hexpm",
    "57e50e4d0baf47c914f1212c6a5e7e95d420d15429fd21b574bbcc40f0b0ff06"
  },
  "phoenix_live_reload": {
    :hex,
    :phoenix_live_reload,
    "1.4.0",
    "fb4d574513b5651f67a04f97e76f44a73a30b83995bb57fa7291a6acfe3e2cc9",
    [:mix],
    [],
    "hexpm",
    "76c2dd748b3523908048a53ec31b6fbb95fe9dabbb8a3b0dc16ad0c1bf04e99c"
  },
  "phoenix_pubsub": {
    :hex,
    :phoenix_pubsub,
    "2.1.0",
    "78c46e48ff34997ac11480ff04d20d2f086e7b95d3cb1332b165ed3fb6b931e2",
    [:mix],
    [],
    "hexpm",
    "45514a5aeb324e70235a8b0489a7872bfb6daa2b8a32e58a5c6e2271e82f1b73"
  },
  "plug": {
    :hex,
    :plug,
    "1.11.0",
    "24e5823682af27015eb3fa3129d08eaffb86e2a5be7029157b9c0855a2251fb4",
    [:mix],
    [
      {:mime, "~> 1.0", [hex: :mime, repo: "hexpm", optional: false]}
    ],
    "hexpm",
    "e0e7c331b3ae3af0112f4542e75b3fc54dbd8636241577989a54c27ab97f4c9b"
  },
  "plug_cowboy": {
    :hex,
    :plug_cowboy,
    "2.7.0",
    "a4d65ff9c114c70a4ab681db32a5b9e41dbfa30c02e86361e2f90be686afef92",
    [:mix],
    [
      {:cowboy, "~> 2.0", [hex: :cowboy, repo: "hexpm", optional: false]},
      {:plug, "~> 1.0", [hex: :plug, repo: "hexpm", optional: false]}
    ],
    "hexpm",
    "cfc007e1a13a3351e97e36ab31126a44e68c6f417eac8f5a5d9b3e77e9c24bd4"
  },
  "poison": {
    :hex,
    :poison,
    "4.0.2",
    "3e12b7abfc7b54df5b8f3ae8b6d4f9da1fa3c606ebf47dc7331a750c018f04fd",
    [:mix],
    [],
    "hexpm",
    "2aa8f95fbfb66c1ac62349f63dbb04ac36353953ef4b24ec3e193b53b8ccfe09"
  },
  "telemetry": {
    :hex,
    :telemetry,
    "0.4.3",
    "7b05fbb4db5b32fc0f8aa5c4a2040348b4aa36687100fb8837b850e90cf60e06",
    [:mix],
    [],
    "hexpm",
    "f6b37a5d1c6c3e3d20497b03293be7f83b46f89a6f3987cc1f9262d299f1eaa7"
  }
}
"""


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
            ("poison", "4.0.2"),
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
            ("telemetry", "0.4.3"),
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
    assert result[0] == ("websock_adapter", "0.5.5")
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
    assert result[0] == [("acceptor_pool", "1.0.0"), ("bamboo", "2.3.0")]
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
        ("bamboo_smtp", "4.2.2"),
        ("bypass", "2.1.0"),
        ("castore", "1.0.5"),
        ("cloak", "1.1.2"),
        ("combination", "0.0.3"),
        ("cowboy", "2.10.0"),
        ("cowboy_telemetry", "0.4.0"),
        ("credo", "1.7.3"),
    ]
    assert not result[1]
