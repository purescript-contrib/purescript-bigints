let upstream =
      https://raw.githubusercontent.com/purescript/package-sets/prepare-0.15/src/packages.dhall
        sha256:0c9c7cd174b7529f2862d52fbfee8cffe1ba8b9cfed3f0f600bdb5be0a14c0ff

in  upstream
  with metadata.version = "v0.15.0-alpha-02"
