class Semgrep < Formula
  include Language::Python::Virtualenv
  desc "Like grep but for code"
  homepage "https://github.com/returntocorp/sgrep"

  head "https://github.com/returntocorp/sgrep.git", :branch => "develop"

  stable do
    url "https://github.com/returntocorp/sgrep/archive/v0.4.9.tar.gz"
    sha256 "e84eed277e58398f18a345756d533cadf4dea28025a4c4dc2ce47232997e7ad4"

    # To avoid upstream breakage, patch the Python code to make it well behaved
    patch do
      url "https://github.com/returntocorp/sgrep/commit/f71a9ccb520c9f7a4eddc93e3e0f80c946741c3d.diff?full_index=1"
      sha256 "3568550fb88764a596fef71728ec0e91d1716e29b049788ea7e2c3d2d9569c87"
    end
  end

  depends_on "coreutils"
  depends_on "python@3.8"

  resource "certifi" do
    url "https://files.pythonhosted.org/packages/b8/e2/a3a86a67c3fc8249ed305fc7b7d290ebe5e4d46ad45573884761ef4dea7b/certifi-2020.4.5.1.tar.gz"
    sha256 "51fcb31174be6e6664c5f69e3e1691a2d72a1a12e90f872cbdb1567eb47b6519"
  end

  resource "chardet" do
    url "https://files.pythonhosted.org/packages/fc/bb/a5768c230f9ddb03acc9ef3f0d4a3cf93462473795d18e9535498c8f929d/chardet-3.0.4.tar.gz"
    sha256 "84ab92ed1c4d4f16916e05906b6b75a6c0fb5db821cc65e70cbd64a3e2a5eaae"
  end

  resource "colorama" do
    url "https://files.pythonhosted.org/packages/82/75/f2a4c0c94c85e2693c229142eb448840fba0f9230111faa889d1f541d12d/colorama-0.4.3.tar.gz"
    sha256 "e96da0d330793e2cb9485e9ddfd918d456036c7149416295932478192f4436a1"
  end

  resource "idna" do
    url "https://files.pythonhosted.org/packages/ad/13/eb56951b6f7950cadb579ca166e448ba77f9d24efc03edd7e55fa57d04b7/idna-2.8.tar.gz"
    sha256 "c357b3f628cf53ae2c4c05627ecc484553142ca23264e593d327bcde5e9c3407"
  end

  resource "PyYAML" do
    url "https://files.pythonhosted.org/packages/3d/d9/ea9816aea31beeadccd03f1f8b625ecf8f645bd66744484d162d84803ce5/PyYAML-5.3.tar.gz"
    sha256 "e9f45bd5b92c7974e59bcd2dcc8631a6b6cc380a904725fce7bc08872e691615"
  end

  resource "requests" do
    url "https://files.pythonhosted.org/packages/01/62/ddcf76d1d19885e8579acb1b1df26a852b03472c0e46d2b959a714c90608/requests-2.22.0.tar.gz"
    sha256 "11e007a8a2aa0323f5a921e9e6a2d7e4e67d9877e85773fba9ba6419025cbeb4"
  end

  resource "urllib3" do
    url "https://files.pythonhosted.org/packages/09/06/3bc5b100fe7e878d3dee8f807a4febff1a40c213d2783e3246edde1f3419/urllib3-1.25.8.tar.gz"
    sha256 "87716c2d2a7121198ebcb7ce7cccf6ce5e9ba539041cfbaeecfb641dc0bf6acc"
  end

  resource "ocaml-binary" do
    url "https://github.com/returntocorp/sgrep/releases/download/v0.4.9/sgrep-0.4.9-osx.zip"
    sha256 "7e710b5c912dfadb0919349b3e5fc60570aba12eb78313ad37adb1487263d018"
  end

  def install
    (buildpath/"ocaml-binary").install resource("ocaml-binary")
    cp "ocaml-binary/sgrep", "ocaml-binary/sgrep-core"
    cp "ocaml-binary/sgrep", "ocaml-binary/semgrep-core"
    bin.install "ocaml-binary/sgrep-core"
    bin.install "ocaml-binary/semgrep-core"

    if build.head?
      python_path = "semgrep"
    else
      python_path = "sgrep_lint"
    end

    cd python_path do
      venv = virtualenv_create(libexec, Formula["python@3.8"].bin/"python3.8")
      python_deps = resources.reject do |resource|
        resource.name == "ocaml-binary"
      end
      venv.pip_install python_deps
      venv.pip_install_and_link buildpath/python_path
    end
  end
  test do
    system "#{bin}/semgrep --help"
    (testpath/"script.py").write <<~EOS
      def silly_eq(a, b):
        return a + b == a + b
    EOS

    output = shell_output("#{bin}/semgrep script.py -l python -e '$X == $X'")
    assert_match "a + b == a + b", output
  end
end
