# Homebrew
This directory contains the code to build homebrew formulas for semgrep.

## How it works
The formula works in two parts:
1. We download the ocaml binary as a resource. We _currently_ need to manually attach this to the release.
2. We rename `sgrep` to `sgrep-core` and install it into the symlinked binary path homebrew provides.
3. We apply this [patch](https://github.com/returntocorp/sgrep/compare/develop...brewable). This makes the `sgrep_lint` directory brew installable and modifies it to look for `sgrep-core` instead of `sgrep`
4. We pip install the package into a `virtualenv`, then link the resulting bin folder of the virtual env.

**Note: as defined in `setup.py`, the executable is called sgrok until someone tells me otherwise. It cannot be called `sgrep`.** When you want to change this, you'll want to update the patch and change the checksum.

## Updating the brew formula for a new release
It takes about 5 minutes. I'm not sure if this is worth automating at the moment. 
0. Bump the version on the formula. If you do not do this, `brew upgrade` will not work for people.
1. Change this block to point to the new release.
```ruby
  stable do
    url "https://github.com/returntocorp/sgrep/archive/v0.4.9.tar.gz"
    sha256 "7820716c96bb85a07ed5e561f66b3fb0cca59e5c910370c58ec04276f99864c5"
  end
```
You can precompute the checksum:
```
curl -L https://github.com/returntocorp/sgrep/archive/v0.4.9.tar.gz | sha256sum
```

2. Change this block to pull in the new ocaml binary:
```ruby
  resource "ocaml-binary" do
    url "https://github.com/returntocorp/sgrep/releases/download/v0.4.9/sgrep-0.4.9-osx.zip"
    sha256 "7e710b5c912dfadb0919349b3e5fc60570aba12eb78313ad37adb1487263d018"
  end
```


## Testing
The easiest way to test a formula is to run brew directly on a file. If you have the file locally, you can literally run:
```
brew install ./semgrep.rb
brew test ./semgrep.rb
```

Coincidentally, this is what the automated testing will also do.
This also works with URLS:
```
brew install https://raw.githubusercontent.com/returntocorp/sgrep/e77e44a948333a968b694bf77b2bf8bf0c3d2920/HomebrewFormula/sgrep-r2c.rb
```

If you have the formula locally, you may need to uninstall it first before running this.

### If it's broken
```
brew install --verbose --debug <formula>
```
