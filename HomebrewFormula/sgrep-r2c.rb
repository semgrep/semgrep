class SgrepR2c < Formula
  version 'v0.4.9b5a'
  #keg_only "maybe?"
  desc "Like grep but for code"
  homepage "https://github.com/r2c/sgrep"
  depends_on "coreutils"
  conflicts_with "sgrep", :because => "we currently include an sgrep binary"

  if OS.mac?
      url "https://github.com/returntocorp/sgrep/releases/download/v0.4.9b5/sgrep-osx-20b13243a29b4df3176648058df432ef63df7a92.zip"
      sha256 "49db8437c428ad5bc92828996440609f112447651e19697a90504140e1307101"
  end

  def install
    # YOLO. This installs all the libraries into bin/ because libraries
    # aren't linked properly. /shrug.
    bin.install Dir["*"]
  end
end
