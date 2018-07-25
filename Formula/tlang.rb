class Tlang < Formula
  desc "Installs the compiler and accompanying REPL for tlang"
  homepage "https://www.tlang.io"
  url "https://github.com/timlindeberg/T-Compiler/releases/download/v0.1/tlang-0.1.tar.gz"
  sha256 "35f1a0cbdf512d9de915e2155ea068bea172bedf284e6195b417484715aa8fa7"
  
  bottle :unneeded

  depends_on :java => "1.8+"

  def install
    libexec.install "bin"
    libexec.install "lib"
    libexec.install "stdlib"

    ["tcompile", "trepl", "t"].each { |binary| 
      (bin/binary).write <<~EOS
        #!/bin/sh
        exec "#{libexec}/bin/#{binary}" "$@"
      EOS
    }
  end

  test do
    # `test do` will create, run in and delete a temporary directory.
    #
    # This test will fail and we won't accept that! For Homebrew/homebrew-core
    # this will need to be a test that verifies the functionality of the
    # software. Run the test with `brew test tlang`. Options passed
    # to `brew install` such as `--HEAD` also need to be provided to `brew test`.
    #
    # The installed folder is not in the path, so use the entire path to any
    # executables being tested: `system "#{bin}/program", "do", "something"`.
    system "false"
  end
end
